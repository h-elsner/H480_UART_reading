{********************************************************}
{                                                        }
{       Read and send data to Yuneec CGO3+ camera        }
{                                                        }
{       Copyright (c) 2024/2025    Helmut Elsner         }
{                                                        }
{       Compiler: FPC 3.2.3   /    Lazarus 3.7           }
{                                                        }
{ Pascal programmers tend to plan ahead, they think      }
{ before they type. We type a lot because of Pascal      }
{ verboseness, but usually our code is right from the    }
{ start. We end up typing less because we fix less bugs. }
{           [Jorge Aldo G. de F. Junior]                 }
{********************************************************}

(*
This source is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your option)
any later version.

This code is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

A copy of the GNU General Public License is available on the World Wide Web
at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1335, USA.

*******************************************************************************)


(*
Recording Async Serial with Saleao Logicanalyzer
Add analyzer Async Serial for each channel
Both bitrate: 115200


Auswertung von MAVlink V1 Messages von der CGO3+ Kamera
Format:
Time [s]	       Value	Parity Error	Framing Error
0.084242666666667      0xFE

MAVlink V1

0.084242666666667	0xFE			V1 magic ($FE)
0.084329583333333	0x03			len	n=0 n/a; n=1 6; n>1 5+n
0.084416500000000	0xEA			sequence no
0.084503583333333	0x03			SysID
0.084590333333333	0x00			Comp ID
0.084677333333333	0x00			MSG ID
0.084764250000000	0x00			pay4
0.084851250000000	0x01			pay5
0.084938166666667	0x01			pay6
0.085025083333333	0x00			pay7
0.085112083333333	0x01			pay8
0.085198916666667	0xE7			crc low
0.085285916666667	0xB8			crc high

https://mavlink.io/en/guide/serialization.html#v1_packet_format


Yuneec Format:
Camera Heartbeat:
0xFE	Magic MAVlink V1                            0
0x05	Length payload (n=0 n/a; n=1 6; n>1 5+n)    1
0x00	Sequence number                             2
0x02	System ID (2 is camera)                     3
0x00	Component ID (Looks like always zero)       4
0x00	Target ID (0 Broadcast)                     5
0x00	SubTarget ID (Looks like always zero)       6
0x00	Message ID (0 - Heartbeat)                  7
0x00	?                                           8
0x00	?                                           9
0x7D	? Status flags?                             10
0x00	?                                           11
0x01	? probably could be MAV Version 1           12
0x3D	CRC low                                     13
0x40	CRC high                                    14


Raw:
Time [s],Value,Parity Error,Framing Error
0.084242666666667,0xFE,,


From MavLinkPackage.java:

Byte0                    FE
Byte1                    len
Byte2                    Sequence No
Byte3                    4
Byte4                    0
byte5                    TargetID
byte6                    targetSubID
byte7                    MsgID
byte8 .. n               Payload

https://mavlink.io/en/guide/serialization.html#v1_packet_format

https://mavlink.io/en/messages/common.html#mav_commands

--------------------------------------------------------------------
StatusBar information:
StatusBar1.Panels[0].Text: Anzahl Zeilen Inlist, and if needed Anzahl Dateien bei Merge, Addlist Zeilen, Message counter
StatusBar1.Panels[1].Text: Format info; Message Format
StatusBar1.Panels[2].Text: Output Format
StatusBar1.Panels[3].Text: Filter gesetzt?
StatusBar1.Panels[4].Text: Dateinamen, Status- und Fehlermeldungen

File format:
0: Invalid
1: Other Saleae file (merge not possible but convert to binary file)
2: YMAV message
3: SR24 message

--------------------------------------------------------------------
2024-07-05 V1.0 First working version
2024-07-06 V1.1 Filter added
2024-08-14 V1.2 SR24 UART support added
2024-08-17      Merge CGO3+ and SR24 communication
2024-08-23      Code review and clean up
2024-08-25 V1.3 Chart added
2024-09-04 V1.4 YGC messages added
2024-09-14      BC Message types reworked
2025-02-20 V1.5 CRC extra and CRC check for FD messages added
2025-04-22 V1.6 Autofilter added - double click on a value in the table
2025-05-10 V1.7 H920 message ID 5 (H920 telemetry) added,
                bug fix for better SR24 file format identification

*)



(*
Used in Main:

function TForm1.CheckRawFileFormat(fn: string): byte;
function UARTreadMsg(var data: TPayLoad; list: TStringList; var idx: integer): boolean;   {Detect and read one message from data stream}
function NewYMavMessage(var list: TStringList; var lineidx: integer): TMAVmessage;        {Return a YMAV message}
function NewV1MavMessage(var list: TStringList; var lineidx: integer): TMAVmessage;       {ToDo}

function TForm1.DoFilterYMAV(msg: TMAVmessage): boolean;
function TForm1.DoFilterSR24(msg: TPayload): boolean;

*)

unit uart_common;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, grids;

const
  AppName='CGO3+/SR24 UART';
  AppVersion='V1.7 2025-05-16';
  meinName='H. Elsner';
  homepage='http://h-elsner.mooo.com';


  hexid='$';
  msgtpformat='nn:ss.zzz';
  secpd=86400;                                     {seconds per day}
  MinimalBytesPerMessage=6;

  UNIXTime=25569;                                  {1.1.1970 00:00 relative to Lazarus Time}
  MAVTime=42005;                                   {1.1.2015 00:00 relative to Lazarus Time}

  v1magicbyte=$FE;
  sr24magic=$55;
  MagicQ500=$FA;

type TByteInfo = record
       ByteStr: string[2];
       AsByte: byte;
       nextByte1: byte;
       nextByte2: byte;
       nextByte3: byte;
       MagicByte: byte;
       len: integer;
       MsgID: byte;
       SysID: byte;
       TargetID: byte;   {= Componente ID}
       ActionType: byte;
       index: integer;
       fix: boolean;
     end;

{Compnent ID's:
https://mavlink.io/en/messages/common.html#MAV_COMPONENT}

{Public functions and procedures}
function HexStrValueToInt(v: string): byte;
function SecondsToDateTime(SaleaeTime: string): TDateTime;
function SaleaeTimeToOutputTime(tp: string): string;   {to min:sec.zzz}
function CharOutput(b: byte): string;
function ASCIIOutput(b: byte): string;
function DTtoSek(const dt: TDateTime): uint64;         {Date/Time in Anzahl Sekunden}
function FillByteInfo(gridraw: TStringGrid; aCol, aRow: integer): TByteInfo;

function GetInt16(b1, b2: byte): int16;
function GetUInt16(b1, b2: byte): uint16;
function GetInt32(b1, b2, b3, b4: byte): int32;
function GetUInt32(b1, b2, b3, b4: byte): uint32;
function GetFloat(b1, b2, b3, b4: byte): single;
function SetLengthTime(tp: string; len: byte=20): string;



implementation

function HexStrValueToInt(v: string): byte;
var
  ValueAsText: string;

begin
  ValueAsText:=StringReplace(trim(v), '0x', hexid, []);
  result:=StrToIntDef(ValueAsText, 0) and 255;
end;   

function SecondsToDateTime(SaleaeTime: string): TDateTime;
var
  nums: double;

begin
  result:=0;
  nums:=StrToFloat(SaleaeTime);
  result:=nums/secpd;
end;

function SaleaeTimeToOutputTime(tp: string): string;   {to min:sec.zzz}
begin
  result:='';
  if length(tp)>2 then
    result:=FormatDateTime(msgtpformat, SecondsToDateTime(tp), [fdoInterval]);
end;

function CharOutput(b: byte): string;
begin
  if (b>31) and (b<127) then begin
    result:=chr(b);
  end else
    result:=IntToHex(b, 2);

end;

function ASCIIOutput(b: byte): string;
begin
  if b=0 then exit(' ');
  if (b>31) and (b<127) then
    result:=chr(b)
  else
    result:='.';
end;

function DTtoSek(const dt: TDateTime): uint64;        {Date/Time in Anzahl Sekunden}
begin
 result:=round((dt-UNIXTime)*secpd);
end;

function FillByteInfo(gridraw: TStringGrid; aCol, aRow: integer): TByteInfo;
var
  p: integer;

  procedure v1magicbyteInfo;
  begin
    result.MsgID:=StrToIntDef(hexid+gridraw.Cells[8, aRow], 0);
    result.SysID:=StrToIntDef(hexid+gridraw.Cells[4, aRow], 0);
    result.TargetID:=StrToIntDef(hexid+gridraw.Cells[6, aRow], 0);
    result.MsgID:=StrToIntDef(hexid+gridraw.Cells[8, aRow], 0);

    if result.MsgID=255 then begin
      result.magicByte:=$BC;
      result.len:=StrToIntDef(hexid+gridraw.Cells[10, aRow], 0);
      result.index:=aCol+5;
      result.ActionType:=StrToIntDef(hexid+gridraw.Cells[14, aRow], 0);
      result.fix:=result.index<15;
    end else begin                                     {has no ActionType}
      result.len:=StrToIntDef(hexid+gridraw.Cells[2, aRow], 0);
      result.index:=aCol+2;
      result.fix:=result.index<9;
    end;
  end;

  procedure SR24magicInfo;
  begin
    result.MsgID:=StrToIntDef(hexid+gridraw.Cells[4, aRow], 0);
    result.len:=StrToIntDef(hexid+gridraw.Cells[3, aRow], 0);
    result.index:=aCol+3;
    result.fix:=aCol<10;

    case result.msgID of
      2:                                             {Telemetry}
        begin
          result.index:=aCol;
          result.fix:=aCol<7;
        end;
      4:                                             {Bind}
        begin
          result.index:=aCol;
          result.fix:=aCol<7;
        end;
      20:                                            {Additional data}
        begin
          result.ActionType:=StrToIntDef(hexid+gridraw.Cells[5, aRow], 0);
          result.index:=aCol;
          result.fix:=result.index<6;
        end;
    end;
  end;

  procedure MagicQ500Info;
  begin
    result.MsgID:=StrToIntDef(hexid+gridraw.Cells[5, aRow], 0);
    result.len:=StrToIntDef(hexid+gridraw.Cells[4, aRow], 5);
    result.index:=aCol-1;
    result.fix:=aCol<6;
  end;

  procedure GB203Info;
  begin
    result.MsgID:=StrToIntDef(hexid+gridraw.Cells[3, aRow], 0);
    result.len:=35;
    result.index:=aCol-3;
  end;

begin
  result:=Default(TByteInfo);
  result.MsgID:=88;
  result.SysID:=88;
  result.TargetID:=88;
  result.ActionType:=88;

  if aCol<gridraw.ColCount-2 then begin
    result.MagicByte:=StrToIntDef(hexid+gridraw.Cells[1, aRow], 0);
    case result.MagicByte of
      v1magicbyte: v1magicbyteInfo;
      SR24magic: SR24magicInfo;
      MagicQ500: MagicQ500Info;
      $88: GB203Info;
    end;

    result.ByteStr:=gridraw.Cells[result.index, aRow];
    result.AsByte:=StrToIntDef(hexid+result.ByteStr, 0);
    p:=result.index+1;
    if p=gridraw.ColCount then
      exit;
    result.nextByte1:=StrToIntDef(hexid+gridraw.Cells[p, aRow], 0);
    inc(p);
    if p=gridraw.ColCount then
      exit;
    result.nextByte2:=StrToIntDef(hexid+gridraw.Cells[result.index+2, aRow], 0);
    inc(p);
    if p=gridraw.ColCount then
      exit;
    result.nextByte3:=StrToIntDef(hexid+gridraw.Cells[result.index+3, aRow], 0);
  end;
end;

function GetInt16(b1, b2: byte): int16;
begin
  result:=b1+256*b2;
end;

function GetUInt16(b1, b2: byte): uint16;
begin
  result:=b1+256*b2;
end;

function GetInt32(b1, b2, b3, b4: byte): int32;
begin
  result:=b1+256*b2+b3*65536+b4*1677216;
end;

function GetUInt32(b1, b2, b3, b4: byte): uint32;
begin
  result:=b1+256*b2+b3*65536+b4*1677216;
end;

function GetFloat(b1, b2, b3, b4: byte): single;
var wfl: packed array[0..3] of Byte;            {Kann auch NaN zurückgeben!}
    wx: Single absolute wfl;

begin
  result:=0;
  wfl[0]:=b1;
  wfl[1]:=b2;
  wfl[2]:=b3;
  wfl[3]:=b4;
  result:=wx;                                    {Typecast mittels absolute}
end;

function SetLengthTime(tp: string; len: byte=20): string;
begin
  result:=tp;
  repeat
    result:='0'+result;
  until
    length(result)>len-1;
end;

end.

