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
  SysUtils;

const
  msgtpformat='nn:ss.zzz';
  secpd=86400;                                      {seconds per day}
  MinimalBytesPerMessage=6;

  UNIXTime=25569;                                  {1.1.1970 00:00 relative to Lazarus Time}
  MAVTime=42005;                                   {1.1.2015 00:00 relative to Lazarus Time}


{Public functions and procedures}
function HexStrValueToInt(v: string): byte;
function SecondsToDateTime(SaleaeTime: string): TDateTime;
function SaleaeTimeToOutputTime(tp: string): string;   {to min:sec.zzz}
function CharOutput(b: byte): string;
function ASCIIOutput(b: byte): string;
function DTtoSek(const dt: TDateTime): uint64;        {Date/Time in Anzahl Sekunden}


implementation

function HexStrValueToInt(v: string): byte;
var
  ValueAsText: string;

begin
  ValueAsText:=StringReplace(trim(v), '0x', '$', []);
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
  if (b>31) and (b<127) then
    result:=chr(b)
  else
    result:=IntToStr(b);
end;

function ASCIIOutput(b: byte): string;
begin
  if (b>31) and (b<127) then
    result:=chr(b)
  else
    result:='.';
end;

function DTtoSek(const dt: TDateTime): uint64;        {Date/Time in Anzahl Sekunden}
begin
 result:=round((dt-UNIXTime)*secpd);
end;




end.

