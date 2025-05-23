{********************************************************}
{                                                        }
{ Read UART data from Logic analyzer at Yuneec drones    }
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
Auswertung von MAVlink V1 Messages von der CGO3+ Kamera

Recording Async Serial with Saleao Logicanalyzer
Add analyzer Async Serial for each channel
Both bitrate: 115200

File type stored in btnDecode.Tag


https://yuneecpilots.com/threads/cgo3-serial-link.18528/#post-208460


*)

unit cam_uart_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Menus, ActnList, Grids, XMLPropStorage, Buttons, Spin, TAGraph,
  math, TAIntervalSources, TASeries, TATransformations, dateutils, mav1defs,
  SR24_dec, uart_common, Types;

Const
  sep=',';
  dtsep=';';
  msgsep=': ';
  csvext='.csv';
  tab1=' ';
  tab2='  ';
  leer='&';        {Identifier not to output to stringgrids}
  docufix=28;
  trenner='====================================';

  SaleaeHeader='Time [s],Value,Parity Error,Framing Error';
  rsSaved='File saved to: ';
  rsAdr='Adr';
  rsMsgID='Msg_ID';
  rsMsgType='Msg_Type';
  rsSeqNo='Seq_No';
  rsSysID='Sys_ID';
  rsTargetID='Target_ID';
  rsBIND='BIND';
  rsPL='PL';
  rsMagic='Magic';
  rsSensor='Sensor';
  rsTime='Time since boot';
  rsLenSequSysID=';Len;Seq_No;Sys_ID;';
  rsLoad='Load a logic analyzer recording of UART from ';
  rsDecode=' to decode messages';
  rsSave='Save the data from the ';
  rsCSVFile=' as CSV file';

  rsLat='Lat';
  rsLon='Lon';
  rsAlt='Alt';
  rsAltMSL='Alt MSL';
  rsNumSats='Num sats';

  capFilter='Filter';
  capSender='Sender';
  capTarget='Target';

  titDecode='Load file to decode...';
  titConvert='Load file to convert to binary...';
  titFilterUSBrec='Load file from USB recording...';
  titMerge='Select CSV files to decode...';
  titSaveRaw='Save Raw table as CSV file...';
  titSaveData='Save Data table as CSV file...';
  titHexView='Load file for hex view...';
  titSensorFile='Save sensor file...';

  errInvalidFile=' is an invalid file';
  errFilter='No match found - check filter criteria!';
  errUnknownMsg=' has unknown message format -> no action';

  hntSaveRaw=rsSave+'"Raw" table'+rsCSVFile;
  hntSaveData=rsSave+'"Data" table'+rsCSVFile;
  hntConvert='Convert a recording from Saleae Logic Analysator in binary or text file';
  hntCreateTimestamp='Create an example for a time stamp and search it';
  hntMerge='Select files from different channels of one recording of the logic analyzer';
  hntResetAll='Clear filter criteria';
  hntClearSearch='Clear search criteria';
  hntClose='Close application';
  hntReLoad='Reload the same files with new filter';
  RawGridHeader=rsTime+dtsep+rsMagic;                        {Use dtsep (;)}
  DataMergeHeader=rsTime+dtsep+rsSeqNo+dtsep+rsMsgType+';ActionType;Sys_ID;Target_ID;RSSI[%]';
  DataBCHeader=RawGridHeader+rsLenSequSysID+'Target_ID;Message name';

  RawSR24Header='Hdr1;Hdr2;Len;Msg_Type;Counter/Action;_?_;RSSI;PackageCtnr';
  SR24DataHeader=rsTime+dtsep+rsMsgType+dtsep+'ActionType;RSSI[%]';

  RawMavHeaderCommon=rsMagic+rsLenSequSysID+'Comp_ID;';
  RawMavHeaderTarget=rsTargetID+';SubTarget_ID;Msg_ID;Msg_Type/PL1';
  YMAVDataHeader=rsTime+dtsep+rsMsgID+dtsep+rsSysID+dtsep+rsTargetID;

  clGridHighlightRows=clForm;
  clMsgID=clGradientActiveCaption;
  clActionType=clGradientActiveCaption;

  ParamFloatFormat='0.0';


type

  { TForm1 }

  TForm1 = class(TForm)
    acSaveRaw: TAction;
    acSaveData: TAction;
    acClose: TAction;
    acConvert: TAction;
    acDecodeUART: TAction;
    acAbout: TAction;
    acResetAll: TAction;
    acHexView: TAction;
    acGoFilter: TAction;
    acReLoad: TAction;
    acProtocol: TAction;
    ActionList1: TActionList;
    btnBack: TBitBtn;
    btnDecode: TButton;
    btnClose: TButton;
    btnDelL: TButton;
    btnDelLM: TButton;
    btnReload: TBitBtn;
    btnResetAll: TButton;
    btnSaveRaw: TButton;
    btnSaveData: TButton;
    btnClearRed: TButton;
    btnSaveProto: TButton;
    cbAutoSave: TCheckBox;
    cbOrOther: TCheckBox;
    Chart1: TChart;
    AutoScaleLeft: TAutoScaleAxisTransform;
    AutoScaleRight: TAutoScaleAxisTransform;
    cbRawWithCRC: TCheckBox;
    cbSensorFile: TCheckBox;
    cbCheckCRC: TCheckBox;
    edErrorCounterMAV: TEdit;
    edErrorCounterSR24: TEdit;
    edID: TEdit;
    edCRCextra: TEdit;
    edLength: TEdit;
    edOther: TEdit;
    edSumMsgID: TEdit;
    edSumMsgType: TEdit;
    gbFilter: TGroupBox;
    gbOtherMsgID: TGroupBox;
    gbSR24Filter: TGroupBox;
    gbExtraTool: TGroupBox;
    lblCRCextra: TLabel;
    lblID: TLabel;
    lblSerialNoOut: TLabel;
    lblSerialNo: TLabel;
    lblDateTimeOut: TLabel;
    lblDateTime: TLabel;
    lbActionType: TListBox;
    lblActionType: TLabel;
    lblLenght: TLabel;
    lblMsgID: TLabel;
    lblMsgType: TLabel;
    lblSensorType: TLabel;
    lblSysID: TLabel;
    lblTarget: TLabel;
    lbMsgID: TListBox;
    lbMsgType: TListBox;
    lbNumMsgID: TListBox;
    lbNumMsgType: TListBox;
    lbSensorType: TListBox;
    Memo1: TMemo;
    mnData: TPopupMenu;
    mnDataSave: TMenuItem;
    mnDoChartLeft: TMenuItem;
    mnDoChartRight: TMenuItem;
    mnSaveProto: TMenuItem;
    plLeft: TPanel;
    rgMsgID: TRadioGroup;
    rgMsgType: TRadioGroup;
    rgOutputFormat: TRadioGroup;
    rgSysID: TRadioGroup;
    rgTargetID: TRadioGroup;
    Separator5: TMenuItem;
    serLeft: TLineSeries;
    serRight: TLineSeries;
    tsText: TTabSheet;
    tsSettings: TTabSheet;
    tsStatistics: TTabSheet;
    transformLeft: TChartAxisTransformations;
    transformRight: TChartAxisTransformations;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    gridData: TStringGrid;
    gridRaw: TStringGrid;
    mnGoFilter: TMenuItem;
    mnMain: TMainMenu;
    mnRawSave: TMenuItem;
    pcMain: TPageControl;
    panLeft: TPanel;
    mnRaw: TPopupMenu;
    rgDataType: TRadioGroup;
    Separator4: TMenuItem;
    mnHexView: TMenuItem;
    Separator3: TMenuItem;
    mnResetFilter: TMenuItem;
    Separator1: TMenuItem;
    mnAbout: TMenuItem;
    mnInfo: TMenuItem;
    mnMerge: TMenuItem;
    mnConvert: TMenuItem;
    mnDecode: TMenuItem;
    mnTools: TMenuItem;
    mnClose: TMenuItem;
    Separator2: TMenuItem;
    mnSaveData: TMenuItem;
    mnSaveRaw: TMenuItem;
    mnFile: TMenuItem;
    OpenDialog: TOpenDialog;
    panButtons: TPanel;
    SaveDialog: TSaveDialog;
    StatusBar1: TStatusBar;
    tsData: TTabSheet;
    tsRaw: TTabSheet;
    tsChart: TTabSheet;
    XMLPropStorage1: TXMLPropStorage;
    procedure acAboutExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure acConvertExecute(Sender: TObject);
    procedure acGoFilterExecute(Sender: TObject);
    procedure acHexViewExecute(Sender: TObject);
    procedure acDecodeUARTExecute(Sender: TObject);
    procedure acProtocolExecute(Sender: TObject);
    procedure acReLoadExecute(Sender: TObject);
    procedure acResetAllExecute(Sender: TObject);
    procedure acSaveDataExecute(Sender: TObject);
    procedure acSaveRawExecute(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnClearRedClick(Sender: TObject);
    procedure btnDelLClick(Sender: TObject);
    procedure btnDelLMClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure gbExtraToolDblClick(Sender: TObject);
    procedure gridDataDblClick(Sender: TObject);
    procedure gridDataGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure gridDataHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure gridDataPrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure gridRawDblClick(Sender: TObject);
    procedure gridRawGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure gridRawHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure gridRawPrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure Memo1MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure Memo1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure mnDoChartLeftClick(Sender: TObject);
    procedure mnDoChartRightClick(Sender: TObject);
    procedure rgDataTypeClick(Sender: TObject);
    procedure rgOutputFormatClick(Sender: TObject);
    procedure rgSysIDClick(Sender: TObject);
    procedure rgTargetIDClick(Sender: TObject);

  private
    function  CheckRawFileFormat(fn: string): byte;
    procedure FillRawSR24Header(const len: integer=maxlen);
    procedure FillSR24DataHeader(len: integer);
    procedure FillRawMAVHeader(len: integer);
    procedure FillMAVDataHeader(headerstring: string; len: integer);
    procedure FillRawGridHeader(len: integer);
    procedure FillDataBCheader(len: integer);
    procedure FillDataMergeHeader(len: integer);
    function  FilenameProposal: string;
    function  DoFilterYMAV(msg: TMAVmessage): boolean;
    function  DoFilterSR24(msg: TPayload): boolean;
    procedure FilterInfo;
    function  OutputFormatByte(b: byte): string;
    procedure CheckIfFilterIsSetMAV;
    procedure CheckIfFilterIsSetSR24;
    procedure ConvertToBinary(fn: string);
    procedure HexViewABinaryFile(fn: string);
    procedure InfoInvalidFile(fn: string);
    procedure InfoSaleaeRecording(fn: string; execute: boolean=false);
    procedure FillgridRaw(var list: TStringList);    {addlist_raw}
    procedure FillgridData(var list: TStringList);    {addlist_data}
    function  GetDataCellInfo(aCol, aRow: integer): string;
    procedure DoChart(chart: byte);        {1 left, 2 right}
    procedure DecodeUART;
    procedure DecodeBCmsg(const msg: TMAVmessage; RowNumber: integer);
    procedure FilterFDmessagesFromUSBrecording(fn: string);
    procedure FilterBCmessagesFromUSBrecording(fn: string);
  public

  end;

var
  Form1: TForm1;
  CRCextra: byte;

implementation

{$R *.lfm}

{ TForm1 }

/////////////////////////////// Internal Routines //////////////////////////////

function SR24UARTreadMsg(var data: TPayLoad; list: TStringList; var idx: integer): boolean;   {Detect and read one message from data stream}
const
  empty: array [0..2] of byte = (0, 0, 0);  {Buffer for header bytes to check if message starts here}

var
  i, z: byte;
  buf: array[0..2] of byte;

  function UARTreadByteFromText: byte;
  var
    s: string;

  begin
    result:=0;
    if idx<list.Count then begin
      repeat
        s:='';
        if trim(list[idx])<>'' then
          s:=trim(list[idx].Split([sep])[1]);
        inc(idx);
      until
        s<>'';
      result:=HexStrValueToInt(s);
    end;
  end;

begin
  result:=false;
  z:=0;                                                  {Counter for unsynced bytes}
  buf:=empty;                                            {Reset buffer}
  repeat
    buf[0]:=UARTreadByteFromText;                        {read byte by byte}
    if (buf[2]=header1) and                              {check if valid message (header+plausible length)}
       (buf[1]=header2) and
       (buf[0]<maxlen) and
       (buf[0]>0) then begin
      data[0]:=buf[2];                                   {Copy header and length to message data}
      data[1]:=buf[1];
      data[2]:=buf[0];
      for i:=3 to buf[0]+2 do                            {Read the other bytes of the dataset (payload + CRC)}
        data[i]:=UARTreadByteFromText;
      z:=0;
      result:=true;                                      {Message is valid but CRC was not checked (no need at this time)}
    end else begin                                       {Shift buffer right}
      buf[2]:=buf[1];
      buf[1]:=buf[0];
      inc(z);                                            {Count bytes to prevent overflow}
    end;
  until result or                                        {Valid message but w/o CRC check}
       (z>maxlen);                                       {Too long message}
end;

function UnknownCRC32(msg: TMAVmessage): uint32;
begin
  result:=0;
//  result:=WinHexCRC32(msg);
end;


function NewYMavMessageFE(list: TStringList; var lineidx: integer): TMAVmessage;
var
  i: integer;

begin
  result:=ClearMAVmessage;
  with result do begin
    if lineidx<list.Count-LengthFixPartFE then begin
      time:=SecondsToDateTime(list[lineidx].Split([sep])[0]);
      msgbytes[0]:=MagicFE;
      inc(lineIdx);
      msgLength:=HexStrValueToInt(list[lineidx].Split([sep])[1]);
      msgbytes[1]:=msgLength;

      if (msglength=0) or (time=0) then begin
        valid:=false;
        exit;
      end;

      if lineidx+LengthFixPartFE+msgLength<list.Count then begin
        for i:=2 to msgLength+LengthFixPartFE+1 do begin
          inc(lineIdx);
          msgbytes[i]:=HexStrValueToInt(list[lineidx].Split([sep])[1]);
        end;
        valid:=true;
      end;

      sysid:=msgbytes[3];
      targetid:=msgbytes[5];
      msgid:=msgbytes[7];
      msgid32:=msgid;
    end;
  end;
  inc(lineIdx);                                          {Next message byte}
end;

function NewYMavMessageFD(list: TStringList; var lineidx: integer): TMAVmessage;
var
  i: integer;

begin
  result:=ClearMAVmessage;
  with result do begin
    if lineidx<list.Count-LengthFixPartFD then begin
      time:=SecondsToDateTime(list[lineidx].Split([sep])[0]);
      msgbytes[0]:=MagicFD;
      inc(lineIdx);
      msgLength:=HexStrValueToInt(list[lineidx].Split([sep])[1]);
      msgbytes[1]:=msgLength;

      if (msglength=0) or (time=0) then begin
        valid:=false;
        exit;
      end;

      if lineidx+msgLength+LengthFixPartFD<list.Count then begin
        for i:=2 to msgLength+LengthFixPartFD+1 do begin
          inc(lineIdx);
          msgbytes[i]:=HexStrValueToInt(list[lineidx].Split([sep])[1]);
        end;
        valid:=true;
      end;

      sysid:=msgbytes[5];
      targetid:=msgbytes[6];
      msgid:=msgbytes[7];
      msgid32:=MavGetUint32(result, 7) and $FFFFFF;
    end;
  end;
  inc(lineIdx);                                          {Next message byte}
end;

function MessageListToStr(list: TStringList): string;
var
  i: integer;

begin
  result:='';
  if list.Count>0 then begin
    result:=list[0];
    for i:=1 to list.Count-1 do
      if list[i]<>leer then
        result:=result+dtsep+list[i];
  end;
end;

procedure FillBinaryHeader(grid: TStringGrid);
var
  i: integer;

begin
  grid.Cells[0, 0]:=rsAdr;
  for i:=0 to 15 do begin
    grid.Cells[i+1, 0]:=IntToStr(i);
    grid.Cells[i+18, 0]:=IntToStr(i);
  end;
  grid.Cells[17, 0]:='ASCII';
  grid.AutoSizeColumns;
end;

procedure HexViewABinaryStream(BinaryStream: TMemoryStream; grid: TStringGrid);
var
  i, aCol, aRow: integer;
  b: byte;

begin
  Screen.Cursor:=crHourglass;
  grid.BeginUpdate;
  try
    grid.ColCount:=34;
    grid.RowCount:=1;                                    {Löschen der vorherigen Anzeigen}
    FillBinaryHeader(grid);
    aCol:=1;
    aRow:=1;
    grid.RowCount:=(BinaryStream.Size div 16) +2;
    grid.Cells[0, 1]:='00000000';
    BinaryStream.Position:=0;
    for i:=1 to BinaryStream.Size-1 do begin
      b:=BinaryStream.ReadByte;
      if aCol=17 then begin
        inc(aRow);
        grid.Cells[0, aRow]:=IntToHex(i-1, 8);
        aCol:=1;
      end;
      grid.Cells[aCol, aRow]:=IntToHex(b, 2);
      grid.Cells[aCol+17, aRow]:=ASCIIoutput(b);
      inc(aCol);
    end;
  finally
    grid.EndUpdate;
    Screen.Cursor:=crDefault;
  end;
end;

function RawMessageToMergelist(Const msg: TPayLoad; timept: string;
                               LengthFixPart: byte; WithCRC: boolean=false): string; overload;
var
  i, num: integer;

begin
  result:=timept;
  num:=msg[2]+1;
  if WithCRC then
    num:=num+2;
  for i:=0 to num do
    result:=result+dtsep+IntToHex(msg[i], 2);
end;

function RawMessageToMergelist(Const msg: TMAVmessage; timept: string;
                               LengthFixPart: byte; WithCRC: boolean=false): string; overload;
var
  i, num: integer;

begin
  result:=timept;
  num:=msg.msgbytes[1]+LengthFixPart-1;
  if WithCRC then
    num:=num+2;
  for i:=0 to num do
    result:=result+dtsep+IntToHex(msg.msgbytes[i], 2);
  if WithCRC then
    result:=result+dtsep+IntToHex(CRC16X25MAV(msg, LengthFixPart, 1, true, CRCextra), 4);
end;

function SR24RawMessageToMergelist(Const msg: TPayLoad; timept: string;
                               LengthFixPart: byte; WithCRC: boolean=false): string;
var
  i, num: integer;

begin
  result:=timept;
  num:=msg[2]+1;
  if WithCRC then
    num:=num+1;
  for i:=0 to num do
    result:=result+dtsep+IntToHex(msg[i], 2);
end;

{for gridData/data CSV the fix part:
  0 Time
  1 Sequ_No
  2 Msg_ID
  3 ActionType
  4 Sys_ID
  5 Target_ID
  6 RSSI
  from 7 on payload}

procedure CreateDataTable(var list: TStringList; tp: string);
var
  i: integer;

begin
  list.Add(tp);
  for i:=1 to 6 do
    list.Add('');
end;

procedure WriteAltMSL(list: TStringList; pos: integer; alt: single);
begin
  list[pos]:=rsAltMSL+'=';
  list[pos+1]:=FloatToAlt(alt)+'m';
  list[pos+2]:='';
  list[pos+3]:='';
end;

procedure WriteAltRel(list: TStringList; pos: integer; alt: int32);
begin
  list[pos]:=rsAlt+'=';
  list[pos+1]:=IntToAlt(alt)+'m';
  list[pos+2]:='';
  list[pos+3]:='';
end;

procedure WriteVxVyVz(list: TStringList; pos: integer; vx, vy, vz: int16);
begin
  list[pos]:='Vx=';
  list[pos+1]:=FormatFloat(ctfl, vx*0.01);
  list[pos+2]:='Vy=';
  list[pos+3]:=FormatFloat(ctfl, vy*0.01);
  list[pos+4]:='Vz=';
  list[pos+5]:=FormatFloat(ctfl, vz*0.01);
end;

procedure WritePRY(list: TStringList; pos: integer; pitch, roll, yaw: single);
begin
  list[pos]:='Pitch=';
  list[pos+1]:=FormatFloat(ctfl, pitch*0.01);
  list[pos+2]:='Roll=';
  list[pos+3]:=FormatFloat(ctfl, roll*0.01);
  list[pos+4]:='Yaw=';
  list[pos+5]:=FormatFloat(ctfl, yaw*0.01);
end;

procedure WriteRCdata(list: TStringList; pos: integer; acc, speed, angle: int16);
begin
  list[pos]:='Accuracy=';
  list[pos+1]:=IntToAccuracy(acc);
  list[pos+2]:='Speed=';
  list[pos+3]:=IntToSpeed(speed);
  list[pos+4]:='Angle=';
  list[pos+5]:=IntToRCAngle(angle);
end;

procedure WriteGPSdata(list: TStringList; pos: integer;
                       lat, lon, alt: single; goalt: boolean=true);
begin
  list[pos]:=rsLat+'=';
  list[pos+1]:=IntToLatLon(CoordToInt(lat));
  list[pos+2]:='';
  list[pos+3]:='';
  list[pos+4]:=rsLon+'=';
  list[pos+5]:=IntToLatLon(CoordToInt(lon));
  list[pos+6]:='';
  list[pos+7]:='';
  if goalt then
    WriteAltMSL(list, pos+8, alt);
end;

function FilterColumn(var aGrid: TStringGrid;
                      const aCol: integer; aText: string; reverse: boolean=false): integer;
var
  i: integer;
  found: boolean;

begin
  result:=0;
  aGrid.BeginUpdate;
  try
    for i:=aGrid.FixedRows to aGrid.RowCount-1 do begin
      if reverse then
        found:=aGrid.Cells[aCol, i]<>aText
      else
        found:=aGrid.Cells[aCol, i]=aText;
      if found then begin
        aGrid.RowHeights[i]:=aGrid.DefaultRowHeight;
        result:=result+1;
      end else
        aGrid.RowHeights[i]:=0;
    end;
  finally
    aGrid.EndUpdate;
  end;
end;

procedure ResetAllFilterColumn(var aGrid: TStringGrid);
var
  i: integer;

begin
  aGrid.BeginUpdate;
  try
    for i:=aGrid.FixedRows to aGrid.RowCount-1 do
      aGrid.RowHeights[i]:=aGrid.DefaultRowHeight;
  finally
    aGrid.EndUpdate;
  end;
end;

function GetRawCellInfo(s: string; aCol, aRow: integer): string;
var
  b: byte;

begin
  if trim(s)='' then
    exit('');
  if aCol=0 then begin
    result:=SaleaeTimeToOutputTime(s);
  end else begin
    if length(s)>2 then
      exit(s);
    b:=StrToIntDef(hexid+s, 0);
    result:='0x'+s+' = '+IntToStr(b);
    if (b>31) and (b<127) and (aCol>4) then
      result:=result+' ('+chr(b)+')';
  end;
end;

function ShowByteAsDefault(b: TByteInfo): string;
var
  s: string;
  v: single;

begin
  with b do begin
    v:=GetFloat(AsByte, nextByte1, nextbyte2, nextbyte3);
    if IsNan(v) then
      s:='NaN'
    else
      s:=FormatFloat(ctfl, v);
    result:='Hex:    '+ByteStr+lineending+
            'Dez:    '+IntToStr(AsByte)+lineending;
    if (AsByte>31) and (AsByte<127) then
            result:=result+
            'Char:   '+Chr(AsByte)+lineending;
            result:=result+
            'Int16:  '+IntToStr(GetInt16(AsByte, nextByte1))+lineending+
            'Uint16: '+IntToStr(GetUInt16(AsByte, nextByte1))+lineending+
            'Int32:  '+IntToStr(GetInt32(AsByte, nextByte1, nextbyte2, nextbyte3))+lineending+
            'Uint32: '+IntToStr(GetUInt32(AsByte, nextByte1, nextbyte2, nextbyte3))+lineending+
            'Float:  '+s;
  end;
end;

function TForm1.GetDataCellInfo(aCol, aRow: integer): string;
var
  ByteInfo: TByteInfo;

begin
  if trim(gridData.Cells[aCol, aRow])='' then
    exit('');

  byteInfo:=FillByteInfo(gridRaw, aCol, aRow);
  result:=ShowByteAsDefault(ByteInfo);              {default}
  with byteinfo do begin
    if MagicByte=sr24magic then begin               {Catch special cases}
      case MsgID of
        4: exit('BIND');

      end;
    end;
  end;
end;

procedure RecreateChart(grid: TStringGrid);
begin
  with grid do begin
    if (ColCount>10) and (RowCount>2) and
       (col>6) and (col<ColCount) and
       (row>0) and (row<RowCount) then
      Form1.DoChart(1);
  end;
end;

//******************************************************************************

procedure MavChannelDataToList(msg: TMAVmessage; var list: TStringList; bytepos, pos: integer; numch: byte);
var
  i: integer;

begin
  for i:=0 to (numch div 2)-1 do begin
    list[i*3+pos]:='CH'+IntToStr(i*2)+'='+IntToStr(MavGetChValue(msg, i*2+1, bytepos));
    list[i*3+pos+1]:='';
    list[i*3+pos+2]:='CH'+IntToStr(i*2+1)+'='+IntToStr(MavGetChValue(msg, i*2+2, bytepos));
  end;
end;

{                                    from byte pos  8   9  10  11  12
00:00.045  62 HEARTBEAT	Camera     All (broadcast) 00  00  7D  00  01
00:00.600 107 HEARTBEAT	Autopilot  All (broadcast) 00  00  00  00  01
                                        to line[x]  7   8   9  10  11}
procedure PayloadHEARTBEAT(msg: TMAVmessage; var list: TStringList); {1Hz}
begin
  // No need
end;

procedure PayloadAttitude(msg: TMAVmessage; var list: TStringList);   {Frequently}
var
  v1, v2, v3: int16;

begin
  if msg.targetid=2 then begin
    // ToDo asap
    MavGetInt123(msg, 8, v1, v2, v3);


    list[21]:='Pan knob=';
    list[22]:=IntToStr(MavGetInt16(msg, 22));
    list[23]:='Tilt=';
    list[24]:=IntToTilt(MavGetInt16(msg, 24));      {OK}
    list[25]:='ToDo26=';
    list[26]:=IntToStr(MavGetInt16(msg, 26));
    list[27]:='Pan Mode:';
    list[28]:=IntToPanMode(MavGetInt16(msg, 28));
    list[29]:='Tilt Mode:';
    list[30]:=IntToTiltMode(MavGetInt16(msg, 30));
    list[31]:='ToDo30=';
    list[32]:=IntToStr(MavGetInt16(msg, 32));
  end;

end;

procedure PayloadTelemetry_5GHz(msg: TMAVmessage; var list: TStringList);
var
  lat, lon, alt: single;
  v1, v2, v3: int16;

  procedure telemetryUp;
  begin
    if MavGetGPSdata(msg, 8, lat, lon, alt) then begin {Get lat, lon and alt from GPS data}
      WriteGPSdata(list, 7, lat, lon, alt, false);
      WriteAltRel(list, 15, MavGetInt32(msg, 15));
    end;

    MavGetInt123(msg, 20, v1, v2, v3);
    WriteVxVyVz(list, 19, v1, v2, v3);
    MavGetInt123(msg, 26, v1, v2, v3);
    WritePRY(list, 25, v1, v2, v3);

    list[31]:='nsat='+IntToStr(msg.msgbytes[32] and $7F);
    if (msg.msgbytes[32] and $80)=$80 then
      list[31]:=list[31]+'*';
    list[32]:=VoltToStr(msg.msgbytes[33]);
    list[35]:='IMU='+IntToHex(msg.msgbytes[36], 2);
    list[36]:='Press/Comp='+IntToHex(msg.msgbytes[37], 2);
    list[37]:=F_ModeToStr(msg.msgbytes[38]);
    list[39]:='ErrFlags='+IntToHex(msg.msgbytes[40], 2);
    list[40]:='GPSaccH=';
    list.Add(GPSaccHToStr(msg.msgbytes[41]));
  end;

  procedure polling;
  var
    i: integer;
    istext: boolean;

  begin
    istext:=true;
    for i:=8 to 13 do
      list[i-1]:=IntToHex(msg.msgbytes[i], 2);
    for i:=14 to msg.msglength do begin
      if istext and (msg.msgbytes[i]>31) and (msg.msgbytes[i]<127) then begin
        list[i-1]:=chr(msg.msgbytes[i]);
      end else begin
        list[i-1]:=IntToHex(msg.msgbytes[i], 2);
        if msg.msgbytes[i]=0 then
          istext:=false;
      end;
    end;
  end;

  procedure Time_ms;
  begin
    list[7]:='';
    list[8]:='Time [ms]';
    list[9]:=IntToStr(MavGetUInt32(msg, 9));
    list[10]:='';
    list[11]:='';
  end;

  procedure MUasGRYPOWER;
  begin
    Time_ms;
  end;

  procedure MUasANGLEEULER;
  begin
    Time_ms;
  end;

  procedure MUasACC;
  begin
    Time_ms;
  end;

  procedure MUasTEMPDIFF;
  begin
    Time_ms;

    list[36]:='IMUtemp=';
    list[37]:=FormatFloat(ctfl, MavGetInt16(msg, 37)*0.01)+'°C';
  end;

  procedure MUasStatus;
  begin
    Time_ms;
    list[12]:='Volt=';
    list[13]:=FormatFloat(ctfl, MavGetUInt16(msg, 13)*0.01);
    list[14]:='Ampere=';
    list[15]:=FormatFloat(ctfl, MavGetUInt16(msg, 15)*0.01);
    list[16]:='Seconds=';
    list[17]:=IntToStr(MavGetUInt16(msg, 17));
    list[18]:='Encdata_P=';
    list[19]:=IntToStr(MavGetInt16(msg, 19));
    list[20]:='Encdata_R=';
    list[21]:=IntToStr(MavGetInt16(msg, 21));
    list[22]:='Encdata_Y=';
    list[23]:=IntToStr(MavGetInt16(msg, 23));
    list[24]:='StageAngle_X=';
    list[25]:=FormatFloat(ctfl, MavGetInt16(msg, 25)*0.01);
    list[26]:='StageAngle_Y=';
    list[27]:=FormatFloat(ctfl, MavGetInt16(msg, 27)*0.01);
    list[28]:='AircraftAngle_X=';
    list[29]:=FormatFloat(ctfl, MavGetInt16(msg, 29)*0.01);
    list[30]:='AircraftAngle_Y=';
    list[31]:=FormatFloat(ctfl, MavGetInt16(msg, 31)*0.01);
    list[32]:='AircraftAngle_Z=';
    list[33]:=FormatFloat(ctfl, MavGetInt16(msg, 33)*0.01);
    list[34]:='Gyrostable_X='+IntToStr(msg.msgbytes[35]);
    list[35]:='Gyrostable_Y='+IntToStr(msg.msgbytes[36]);
    list[36]:='Gyrostable_Z='+IntToStr(msg.msgbytes[37]);
  end;

  procedure YGC_Type18;
  begin
  // unknown
  end;

  procedure FW_Info;
  var
    i: integer;

  begin
    list[7]:=IntToStr(msg.msgbytes[8]);
//    list[7]:='';
    for i:=8 to list.Count-1 do
      if msg.msgbytes[i+1]>31 then
        list[i]:=CharOutput(msg.msgbytes[i+1]);
  end;

  procedure CamYGCmessage;
  begin
    list[3]:=YGC_TypeToStr(msg.msgbytes[8]); {YGC msg type to Action Type: optional}
    list[7]:='';
    case msg.msgbytes[8] of
      1: MUasGRYPOWER;
      2: MUasANGLEEULER;
      3: MUasACC;

      5: MUasTEMPDIFF;
      6: MUasStatus;
      18: YGC_Type18;
      254: FW_Info;
    end;
  end;

  procedure ToolYGCmessage;
  begin
    list[3]:=YGC_CommandToStr(msg.msgbytes[8]);
    {Undef}
  end;

begin
  case msg.targetid of
    0: Polling;
    2: if msg.sysid=10 then ToolYGCmessage;
    3: telemetryUp;
    10: if msg.sysid=2 then CamYGCmessage;
  end;
end;

procedure PayloadGimbalPosition(msg: TMAVmessage; var list: TStringList); {Sys_Id Camera?; 1Hz }
var
  v1, v2, v3: int16;

begin
  MavGetInt123(msg, 8, v1, v2, v3);
  list[7]:='Pan=';
  list[8]:=FormatFloat(dzfl, v1*0.01);         {ToDo asap}
  list[9]:='Tilt=';
  list[10]:=FormatFloat(dzfl,v2*0.01+90);
  list[11]:='Roll=';
  list[12]:=FormatFloat(dzfl, v3*0.01);
end;


procedure PayloadCGPS_5GHz(msg: TMAVmessage; var list: TStringList);
var
  lat, lon, alt: single;
  v1, v2, v3: int16;

begin
  if MavGetGPSdata(msg, 8, lat, lon, alt) then  {Get lat, lon and alt from GPS data}
    WriteGPSdata(list, 7, lat, lon, alt, true);

  MavGetInt123(msg, 20, v1, v2, v3);
  WriteRCdata(list, 19, v1, v2, v3);

  list[25]:=IntToStr(msg.msgbytes[26]);         {Counter}
  MavChannelDataToList(msg, list, 27, 26, 12);
  list[44]:=IntToStr(msg.msgbytes[45]);         {GPS accuracy or something like that as decimal}
end;

{PARAM_REQUEST_READ
https://mavlink.io/en/messages/common.html#PARAM_REQUEST_READ
}
procedure PayloadPARAM_REQUEST_READ(msg: TMAVmessage; var list: TStringList);
var
  i: integer;

begin
  if msg.targetid=5 then begin
    for i:=10 to 25 do
      list[i-1]:=CharOutput(msg.msgbytes[i]);
    list[25]:='Param_Index=';
    list[26]:=IntToStr(MavGetUInt16(msg, 26));
  end;
end;

procedure PayloadAnswerParamRequest(msg: TMAVmessage; var list: TStringList);
begin
   // ToDo low prio
end;

procedure PayloadSensorData(msg: TMAVmessage; var list: TStringList);

  procedure BootTimeIn_ms(listpos, bytepos: integer);
  begin
    list[listpos]:='Since boot:';
    list[listpos+1]:=FormatFloat(mlfl, MavGetUInt32(msg, bytepos)/1000);
    list[listpos+2]:='s';
    list[listpos+3]:='';
  end;

  procedure SYS_STATUS;                  {System_status (1)}
  begin
    list[22]:='Load';
    List[23]:=IntToStr(MavGetUInt16(msg, 26))+'%';
    list[24]:='Batt';
    List[25]:=FormatFloat(dzfl, MavGetUInt16(msg, 28)*0.001)+'V';
    if msg.msgbytes[32]<>$FF then
      list[28]:='Batt '+IntToStr(msg.msgbytes[32])+'%';
  end;

  procedure Sys_Time;                  {2}
  var
    time: TDateTime;
    i: integer;

  begin
    list[10]:='System time:';
    List[11]:=GetSystemTime(msg, 14, time);
    if time>UNIXTime+1000 then begin
      for i:=12 to 17 do
        list[i]:='';
      if Form1.StatusBar1.Panels[5].Text='' then
        Form1.StatusBar1.Panels[5].Text:='System time [UTC]: '+list[11];
    end;
    BootTimeIn_ms(18, 22);
  end;


{
00:00.721	44	SensorData	GPS data (0x18=24)	Autopilot	Camera		01	01	18	E0	11	E3	16	00	00	00	00	0D	B7	BE	1C	C8	98	04	06	26	D2	07	00	4C	00	79	00	06	00	C1	85	03	0D
00:01.121	60	SensorData	GPS data (0x18=24)	Autopilot	Camera		01	01	18	60	2C	E9	16	00	00	00	00	0D	B7	BE	1C	C1	98	04	06	8A	D2	07	00	4C	00	79	00	03	00	C1	85	03	0D
}
  procedure GPS_RAW_INT;                  {18'h 24}
  var
    lat, lon, alt: single;
    i: integer;

  begin
    list[10]:='Since boot:';
    list[11]:=FormatFloat(mlfl, MavGetUInt64(msg, 14)/1000000);
    list[12]:='s';
    for i:=13 to 17 do
      list[i]:='';

    if MavGetGPSdata(msg, 22, lat, lon, alt) then begin {Get lat, lon and alt from GPS data}
      WriteGPSdata(list, 18, lat, lon, alt, false);
    end;
    alt:=MavGetInt32(msg, 30)*0.001;
    list[26]:='Alt_MSL=';
    list[27]:=FormatFloat(ctfl, alt);
    list[28]:='m';
    list[29]:='';
    list[30]:='eph=';
    list[31]:=FormatFloat(ctfl, MavGetUint16(msg, 34)*0.01);
    list[32]:='epv=';
    list[33]:=FormatFloat(ctfl, MavGetUint16(msg, 36)*0.01);
    list[34]:='vel=';
    list[35]:=FormatFloat(ctfl, MavGetUint16(msg, 38)*0.01)+'m/s';
    list[36]:='cog=';
    list[37]:=FormatFloat(ctfl, MavGetUint16(msg, 40)*0.01)+'°';
    list[38]:='FixType: '+IntToStr(msg.msgbytes[42]);
    list[39]:=IntToStr(msg.msgbytes[43])+' Sats';
  end;

  procedure GPS_STATUS;
  begin

  end;

  procedure RAW_IMU;                          {1B'h 27}
  var
    i: integer;

  begin
    list[10]:='Since boot:';
    list[11]:=FormatFloat(mlfl, MavGetUInt64(msg, 14)/1000000)+'s';
    for i:=12 to 17 do
      list[i]:='';

  end;

  procedure SCALED_PRESSURE;                  {1D'h 29}
  begin
    BootTimeIn_ms(10, 14);

    list[14]:='Pressure absolute=';
    list[15]:=FormatFloat(dzfl, MavGetFloatFromBuf(msg, 18));
    list[16]:='hPa';
    list[17]:='';

    list[14]:='Pressure diff=';
    list[15]:=FormatFloat(mlfl, MavGetFloatFromBuf(msg, 22));
    list[16]:='hPa';
    list[17]:='';

    list[22]:='Baro temp=';
    list[23]:=FormatFloat(ctfl, MavGetUInt16(msg, 26)*0.01)+'°C';
  end;

  procedure ATTITUDE;
  begin
    BootTimeIn_ms(10, 14);

  end;

  procedure LOCAL_POSITION_NED;
  begin
    BootTimeIn_ms(10, 14);

  end;

  procedure GLOBAL_POSITION_INT;                  {21'h 33}
  var
    lat, lon, alt: single;

  begin
    BootTimeIn_ms(10, 14);

    if MavGetGPSdata(msg, 18, lat, lon, alt) then begin {Get lat, lon and alt from GPS data}
      alt:=MavGetInt32(msg, 26)*0.001;
      WriteGPSdata(list, 14, lat, lon, alt, true);
    end;
    alt:=MavGetInt32(msg, 30)*0.001;
    list[26]:=rsAlt+'=';
    list[27]:=FormatFloat(ctfl, alt)+'m';
    list[28]:='';
    list[29]:='';

    list[36]:='Heading=';
    list[37]:=FormatFloat(dzfl, MavGetUInt16(msg, 40)*0.01)+'°';
  end;

  procedure RC_CHANNELS_RAW;
  var
    i: integer;

  begin
    BootTimeIn_ms(10, 14);
    for i:=1 to 8 do begin
      list[i*2 + 12]:='Chan'+IntToStr(i);
      list[i*2 + 13]:=IntToStr(MavGetUInt16(msg, i*2 + 16));
    end;
    list[30]:='Port: '+IntToStr(msg.msgbytes[34]);
    list[11]:='RSSI='+IntToStr(msg.msgbytes[35]);
  end;

  procedure SERVO_OUTPUT_RAW;
  var
    i: integer;

  begin
    list[10]:='Since boot:';
    list[11]:=FormatFloat(mlfl, MavGetUInt32(msg, 14)/1000000);
    list[12]:='s';
    list[13]:='';
    for i:=1 to 8 do begin
      list[i*2 + 12]:='Servo'+IntToStr(i);
      list[i*2 + 13]:=IntToStr(MavGetUInt16(msg, i*2 + 16))+'µs';
    end;
    list[30]:='Port: '+IntToStr(msg.msgbytes[34]);
  end;

  procedure MISSION_CURRENT;
  begin
    list[10]:='Sequence:';
    list[11]:=IntToStr(MavGetUInt16(msg, 14));
  end;

  procedure MISSION_REQUEST_INT;                   {3 bytes, unknown}
  begin
    list[10]:='Target: '+IntToStr(MavGetUInt16(msg, 14));

  end;

  procedure Sys_type;
  var
    i: integer;

  begin
    for i:=16 to 34 do begin
      if msg.msgbytes[i+4]>0 then
        list[i]:=chr(msg.msgbytes[i+4]);
    end;
  end;

  procedure NAV_CONTROLLER_OUTPUT;
  begin

  end;

  procedure RC_CHANNELS;
  var
    i: integer;

  begin
    BootTimeIn_ms(10, 14);
    for i:=1 to 18 do begin
      list[i*2 + 12]:='Chan'+IntToStr(i);
      list[i*2 + 13]:=IntToStr(MavGetUInt16(msg, i*2 + 16));
    end;
    list[50]:=IntToStr(msg.msgbytes[54])+' channels';
    list[51]:='RSSI='+IntToStr(msg.msgbytes[55]);
  end;

  procedure VRF_HUD;
  begin
    list[10]:='Airspeed=';
    list[11]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 14));
    list[12]:='m/s';
    list[13]:='';
    list[14]:='Groundspeed=';
    list[15]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 18));
    list[16]:='m/s';
    list[17]:='';
    list[18]:='Alt_rel=';
    list[19]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 22));
    list[20]:='m';
    list[21]:='';
    list[22]:='Climb rate=';
    list[23]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 26));
    list[24]:='m/s';
    list[25]:='';
    list[26]:='Heading [°]=';
    list[27]:=IntToStr(MavGetInt16(msg, 30));
    list[28]:='Throttle [%]=';
    list[29]:=IntToStr(MavGetUInt16(msg, 32));
  end;

  procedure SENSOR_OFFSETS;
  begin

  end;

  procedure AHRS;
  begin

  end;

  procedure HW_STATUS;
  begin
    list[10]:='Vcc_Board=';
    list[11]:=FormatFloat(ctfl, MavGetUInt16(msg, 14)*0.001)+'V';
    list[12]:='I²C_Error: '+IntToStr(msg.msgbytes[16]);
  end;

  procedure DATA96;
  begin
    list[12]:='DataType: '+IntToStr(msg.msgbytes[14]);
    list[13]:='Len='+IntToStr(msg.msgbytes[15]);
  end;

  procedure RANGEFINDER;
  begin
    list[10]:='Distance=';
    list[11]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 14));
    list[12]:='m';
    list[13]:='';
    list[14]:='RawVoltage=';
    list[15]:=FormatFloat(mlfl, MavGetFloatFromBuf(msg, 18));
    list[16]:='V';
    list[17]:='';
  end;

{
00:00.747	50	SensorData	Home? (0xB2=178)	Autopilot	Camera		01	01	B2	C7	41	4C	3C	BC	3A	BC	3B	2F	7F	AA	BF	1E	A5	FF	43	0D	B7	BE	1C	C8	98	04	06
00:01.147	66	SensorData	Home? (0xB2=178)	Autopilot	Camera		01	01	B2	64	CF	49	3C	0C	B6	BC	3B	F8	80	AA	BF	B8	9E	FF	43	0D	B7	BE	1C	C1	98	04	06
}
  procedure AHRS2;                 {Attitude and Heading Reference System}
  var
     lat, lon, alt: single;

  begin
    list[22]:='Alt_MSL=';
    list[23]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 26));
    list[24]:='[m]';
    list[25]:='';
    if MavGetGPSdata(msg, 30, lat, lon, alt) then  {Get lat, lon and alt from GPS data}
      WriteGPSdata(list, 26, lat, lon, alt, false);
  end;

  procedure EKF_STATUS_REPORT;
  begin

  end;

  procedure STATUS_TEXT;
  var
    i: integer;

  begin
    list[10]:='Severity: '+SeverityToStr(msg.msgbytes[14]);
    for i:=11 to 60 do begin
      if msg.msgbytes[i+4]=0 then
        list[i]:='.'
      else
        list[i]:=chr(msg.msgbytes[i+4]);
    end;
  end;

{https://mavlink.io/en/messages/common.html}
begin
  list[9]:=IntToStr(msg.msgbytes[13]);
  case msg.msgbytes[13] of             {Message type BC}
    1:  SYS_STATUS;
    2:  Sys_Time;
    24: GPS_RAW_INT;
    25: GPS_STATUS;
    27: RAW_IMU;
    29: SCALED_PRESSURE;
    30: ATTITUDE;
    32: LOCAL_POSITION_NED;
    33: GLOBAL_POSITION_INT;
    35: RC_CHANNELS_RAW;
    36: SERVO_OUTPUT_RAW;
    42: MISSION_CURRENT;
    51: MISSION_REQUEST_INT;
    52: Sys_type;
    62: NAV_CONTROLLER_OUTPUT;
    65: RC_CHANNELS;
    74: VRF_HUD;
    150: SENSOR_OFFSETS;
    163: AHRS;
    165: HW_STATUS;
    172: DATA96;
    173: RANGEFINDER;
    178: AHRS2;
    193: EKF_STATUS_REPORT;
    253: STATUS_TEXT;
  end;
end;


function YMAVdataToMergelist_FE(msg: TMAVmessage; timept: string): string;
var
  i: integer;
  datalist: TStringList;

begin
  result:='';
  datalist:=TStringList.Create;
  try

// Fix part
    CreateDataTable(datalist, timept);
    datalist[1]:=IntToStr(msg.msgbytes[2]);        {1 Sequ_No}
    datalist[2]:=MsgIDToStr_FE(msg.msgid);         {2 Msg_ID}
    datalist[4]:=SysIDToStr(msg.sysid);            {4 Sys_ID}
    datalist[5]:=TargetIDToStr(msg.targetid);      {5 Target_ID}

// Payload
    if (msg.msgid=255) and (msg.msglength>13) then begin
      datalist[3]:=SensorTypeToStr(msg.msgbytes[13]); {Sensor msg type to Action Type: optional}
      for i:=11 to msg.msglength+5 do  {Only the 'inner' payload of the BC-messages w/o its CRC}
        datalist.Add(Form1.OutputFormatByte(msg.msgbytes[i]))
    end else                               {All other messages}
      for i:=8 to msg.msglength+LengthFixPartFE-1 do
        datalist.Add(Form1.OutputFormatByte(msg.msgbytes[i]));

    case msg.msgid of
      0:   PayloadHEARTBEAT(msg, datalist);             {1Hz}
      1:   PayloadAttitude(msg, datalist);              {Frequently}
      2:   PayloadTelemetry_5GHz(msg, datalist);
      3:   PayloadGimbalPosition(msg, datalist);        {Sys_Id Camera?; 1Hz }
      8:   PayloadCGPS_5GHz(msg, datalist);
      20:  PayloadPARAM_REQUEST_READ(msg, datalist);
//      57:  Payload?
      76:  PayloadAnswerParamRequest(msg, datalist);
      255: PayloadSensorData(msg, datalist);
    end;

    result:=MessageListToStr(datalist);
  finally
    datalist.Free;
  end;
end;

procedure PayloadE90GimbalControl(msg: TMAVmessage; var list: TStringList);
var
  i: integer;
begin
  for i:=0 to 12 do begin
    list[i*2+7]:='W'+IntToStr(i+1)+'=';
    list[i*2+8]:=IntToStr(MavGetInt16(msg, i*2+10));
  end;
  list[21]:='Pan=';
  list[23]:='Tilt=';
  list[27]:='Pan mode=';
  list[29]:='Tilt mode=';
end;


function YMAVdataToMergelist_FD(msg: TMAVmessage; timept: string): string;
var
  i: integer;
  datalist: TStringList;

begin
  result:='';
  datalist:=TStringList.Create;
  try

// Fix part
    CreateDataTable(datalist, timept);
    datalist[1]:=IntToStr(msg.msgbytes[4]);        {1 Sequ_No}
    datalist[2]:=MsgIDtoStr(msg.msgid32);          {2 Msg_ID}
    datalist[4]:=SysIDToStr(msg.sysid);            {4 Sys_ID}
    datalist[5]:=TargetIDToStr(msg.targetid);      {5 Target_ID}

// Payload
    for i:=LengthFixPartFD to msg.msglength+LengthFixPartFD-1 do
      datalist.Add(Form1.OutputFormatByte(msg.msgbytes[i]));

    case msg.msgid32 of
      5000: PayloadE90GimbalControl(msg, datalist);
    end;

    result:=MessageListToStr(datalist);
  finally
    datalist.Free;
  end;
end;


//===========================================================================

procedure ChannelDataToList(msg: TPayload; var list: TStringList; pos: integer; numch: byte);
var
  i: integer;

begin
  for i:=0 to (numch div 2)-1 do begin
    list[i*3+pos]:='CH'+IntToStr(i*2)+'='+IntToStr(GetChValue(msg, i*2+1));
    list[i*3+pos+1]:='';
    list[i*3+pos+2]:='CH'+IntToStr(i*2+1)+'='+IntToStr(GetChValue(msg, i*2+2));
  end;
end;

procedure TelemetryPayload(msg: TPayload; var list: TStringList);
var
  lat, lon, alt: single;
  v1, v2, v3: int16;

begin
  if GetGPSdata(msg, 6, lat, lon, alt) then
    WriteGPSdata(list, 7, lat, lon, alt, false);    {w/o altitude}
  WriteAltRel(list, 15, GetIntFromBuf(msg, 14, 4));

  GetInt123(msg, 18, v1, v2, v3);
  WriteVxVyVz(list, 19, v1, v2, v3);
  GetInt123(msg, 27, v1, v2, v3);
  WritePRY(list, 28, v1, v2, v3);

  list[25]:='nsat='+IntToStr(msg[24] and $7F);
  if (msg[24] and $80)=$80 then
    list[25]:=list[25]+'*';
  list[26]:=VoltToStr(msg[25]);
  list[35]:='IMU='+IntToHex(msg[34], 2);
  list[36]:='Press/Comp='+IntToHex(msg[35], 2);
  list[37]:=F_ModeToStr(msg[36]);
  list[39]:='ErrFlags='+IntToHex(msg[38], 2);
  list[40]:='GPSaccH=';
  list.Add(GPSaccHToStr(msg[39]));

end;

procedure RemoteGPSPayload(msg: TPayload; var list: TStringList);
var
  lat, lon, alt: single;
  v1, v2, v3: int16;

begin
  ChannelDataToList(msg, list, 7, 12);
  if GetGPSdata(msg, 26, lat, lon, alt) then
    WriteGPSdata(list, 25, lat, lon, alt, true);

  GetInt123(msg, 38, v1, v2, v3);
  WriteRCdata(list, 37, v1, v2, v3);

  list[43]:=rsNumSats+'=';
  list.Add(IntToStr(msg[44]));
end;


procedure AdditionalDataPayload(msg: TPayload; var list: TStringList);

  procedure SetSysIDAndTargetIDtoRC;
  begin
    list[4]:=SysIDToStr(1);                     {virtual Sys_ID Autopilot}
    list[5]:=TargetIDToStr(99);                 {virtual Target_ID Remote}
  end;

  procedure SetSysIDAndTargetIDtoAutopilot;
  begin
    list[4]:=SysIDToStr(4);                     {virtual Sys_ID Remote}
    list[5]:=TargetIDToStr(1);                  {virtual Target_ID Autopilot}
  end;

  procedure atREQUEST;
  begin

  end;

  procedure atResponse;
  begin

  end;

  procedure atFEEDBACK;
  begin
    SetSysIDAndTargetIDtoRC;
  end;

  procedure atSETTING_CCC;
  begin

  //    SetSysIDAndTargetIDtoRC;
  end;

  procedure atSETTING_ROI;
  begin

  end;

  procedure atONEKEY_TAKEOFF;
  begin

  end;

  procedure atSETTING_JOUR;
  begin

  end;

  procedure atREALSENSE_DEPTH;
  begin
  // always 0 if not in flight, needs to find out somehow else
    SetSysIDAndTargetIDtoRC;
  end;

  procedure atHomeAltitude;
  begin
    begin
      list[7]:=IntToAlt(GetIntFromBuf(msg, 5, 2))+'m';
      list[8]:='';
      if (msg[7]=0) and (msg[8]=0) then begin
        SetSysIDAndTargetIDtoRC;
        list[9]:='Initial set';
        list[10]:='';
      end;
    end;
  end;

begin
  SetSysIDAndTargetIDtoAutopilot;              {Default: Sent to Autopilot}
  case msg[4] of                               {Action Types (atXXX)}
    0:  atREQUEST;
    1:  atResponse;
    2:  atFEEDBACK;                            {Comes from FC: 0 0 0 }
    3:  atSETTING_CCC;                         {down Gimbaldaten?}
    4:  atSETTING_ROI;
    5:  list[7]:='Sonar '+SwitchPos(msg[5]);   {Sonar switch}
    6:  atONEKEY_TAKEOFF;
    7:  atSETTING_JOUR;
    8:  atREALSENSE_DEPTH;                     {down, all zero}
    9:  list[7]:='LED '+SwitchPos(msg[5]);     {Led switch}
    10: list[7]:='GPS '+GPSswitchToStr(msg[5]); {GPS switch}
    11: atHomeAltitude;
  end;
end;

function SR24dataToMergelist(msg: TPayLoad; timept: string): string;
var
  i: integer;
  lenfix: byte;
  datalist: TStringList;

  procedure ChannelData;
  begin
    CreateDataTable(datalist, timept);
    datalist[1]:=IntToStr(msg[4]);                 {1 Sequ_No}
    datalist[2]:=MessageTypeToStr(msg[3]);         {Msg_ID}
    datalist[4]:=SysIDToStr(4);                    {virtual Sys_ID}
    datalist[5]:=TargetIDToStr(1);                 {virtual 5 Target_ID}
    datalist[6]:=IntToStr(GetRSSI(msg));           {6 RSSI}
  end;

  procedure Telemetry;
  begin
    lenfix:=6;
    CreateDataTable(datalist, timept);
    datalist[1]:=IntToStr(msg[4]+msg[5]*256);      {Sequ_No int16}
    datalist[2]:=MessageTypeToStr(msg[3]);         {Msg_ID}
    datalist[4]:=SysIDToStr(1);                    {virtual Sys_ID}
    datalist[5]:=TargetIDToStr(99);                {virtual Target_ID}
  end;

  procedure Bind;
  begin
    lenfix:=0;
    CreateDataTable(datalist, timept);
    datalist[1]:=IntToStr(msg[4]);                 {Sequ_No}
    datalist[2]:=MessageTypeToStr(msg[3]);         {Msg_ID}
    datalist.Add(rsBIND);                          {payload}
  end;

  procedure AdditionalData;
  begin
    lenfix:=5;                                     {Payload added regarding ActionType}
    CreateDataTable(datalist, timept);
    datalist[2]:=MessageTypeToStr(msg[3]);         {Msg_ID}
    datalist[3]:=ActionTypeToStr(msg[4]);          {ActionType}
  end;

begin
  result:='';
  datalist:=TStringList.Create;
  try
    lenfix:=LengthFixPartFE;     {default lenght of fix part which is in decimal}

    case msg[3] of           {Fix part and payload w/o decoding}
       2: Telemetry;
       4: Bind;
      20: AdditionalData;
    else
      ChannelData;                                {this (0, 1, 3) is seen as default with default lenfix=8}
    end;
    for i:=lenfix to msg[2]+1 do                  {Create payload, possibly overwritten}
      datalist.Add(Form1.OutputFormatByte(msg[i]));

    case msg[3] of                                {decode and overwrite payload}
      0:  ChannelDataToList(msg, datalist, 7, 12);
      1:  ChannelDataToList(msg, datalist, 7, 24);
      2:  TelemetryPayload(msg, datalist);
      3:  RemoteGPSPayload(msg, datalist);
      20: AdditionalDataPayload(msg, datalist);
    end;

    result:=MessageListToStr(datalist);
  finally
    datalist.Free;
  end;
end;


/////////////////////////// Headers //////////////////////////////////////////

function FillFixpartOfHeader(grid: TStringGrid; fixheader: string; pos: integer): integer;
var
  i, headersize: integer;
  headerArr: TStringArray;

begin
  headerArr:=fixheader.Split([dtsep]);
  headersize:=High(headerArr);
  for i:=0 to headersize do
    grid.Cells[i+pos, 0]:=headerArr[i];
  result:=headersize+pos+1;
end;

procedure NumberingColumns(grid: TStringGrid; prefix: string; pos, len: integer);
var
  i: integer;

begin
  for i:=1 to len do begin
    grid.Cells[i+pos-1, 0]:=prefix+IntToStr(i);
  end;
end;

procedure TForm1.FillRawGridHeader(len: integer);
var
  pos: integer;

begin
  gridRaw.RowCount:=1;
  gridRaw.ColCount:=len;
  pos:=FillFixpartOfHeader(gridRaw, RawGridHeader, 0);
  NumberingColumns(gridRaw, 'Byte', pos, len-pos);
  gridRaw.AutoSizeColumns;
end;

procedure TForm1.FillDataBCheader(len: integer);
var
  pos: integer;

begin
  gridData.RowCount:=1;
  gridData.ColCount:=len;
  pos:=FillFixpartOfHeader(gridData, DataBCHeader, 0);
  NumberingColumns(gridData, 'Byte', pos, len-pos);
  gridData.AutoSizeColumns;
end;

procedure TForm1.FillDataMergeHeader(len: integer);
var
  pos: integer;

begin
  gridData.RowCount:=1;
  gridData.ColCount:=len;
  pos:=FillFixpartOfHeader(gridData, DataMergeHeader, 0);
  NumberingColumns(gridData, rsPL, pos, len-pos);
end;

procedure TForm1.FillRawMAVHeader(len: integer);
var
  pos: integer;

begin
  gridRaw.RowCount:=1;
  gridRaw.ColCount:=len;
  pos:=FillFixpartOfHeader(gridRaw, RawMavHeaderCommon+RawMavHeaderTarget, 0);
  NumberingColumns(gridRaw, rsPL, pos, len-pos);
  gridRaw.AutoSizeColumns;
end;

procedure TForm1.FillMAVDataHeader(headerstring: string; len: integer);
var
  pos: integer;

begin
  gridData.RowCount:=1;
  gridData.ColCount:=len;
  pos:=FillFixpartOfHeader(gridData, headerstring, 0);
  NumberingColumns(gridData, rsPL, pos, len-pos);
  gridRaw.AutoSizeColumns;
end;

procedure TForm1.FillRawSR24Header(const len: integer=maxlen);
begin
  gridRaw.RowCount:=1;
  gridRaw.ColCount:=len;
  FillFixpartOfHeader(gridRaw, RawSR24Header, 0);
  NumberingColumns(gridRaw, rsPL, 8, len-8);
  gridRaw.AutoSizeColumns;
end;

procedure TForm1.FillSR24DataHeader(len: integer);
begin
  gridData.RowCount:=1;
  gridData.ColCount:=len;
  FillFixpartOfHeader(gridData, SR24DataHeader, 0);
  NumberingColumns(gridData, rsPL, 4, len-4);
end;

/////////////////////////////// Routines of this class //////////////////////////////

procedure TForm1.InfoInvalidFile(fn: string);
begin
  StatusBar1.Panels[2].Text:=rsUndef;
  StatusBar1.Panels[4].Text:=fn+errInvalidFile;
end;

procedure TForm1.InfoSaleaeRecording(fn: string; execute: boolean=false);
begin
  StatusBar1.Panels[2].Text:='to bin';
  StatusBar1.Panels[4].Text:=fn+' is another Saleae recording';
  if execute then
    ConvertToBinary(fn);
end;

procedure TForm1.FillgridRaw(var list: TStringList);     {addlist_raw}
var
  dataset: TStringList;
  header: TStrings;
  i, maxcolumns: integer;

begin
  dataset:=TStringList.Create;
  try
    dataset.StrictDelimiter:=true;
    dataset.Delimiter:=dtsep;
    gridRaw.RowCount:=1;                   {Löschen der vorherigen Anzeigen}
    gridRaw.RowCount:=list.Count+1;
    maxcolumns:=MinimalBytesPerMessage;
    for i:=0 to list.Count-1 do begin
      dataset.DelimitedText:=list[i];
      dataset[0]:=trim(dataset[0]);
      if dataset.Count>maxcolumns then
        maxcolumns:=dataset.Count;
      gridRaw.Rows[i+1]:=dataset;
    end;
    gridRaw.ColCount:=maxcolumns;

    header:=gridRaw.Rows[0];
    header.StrictDelimiter:=true;
    header.Delimiter:=dtsep;
    list.Insert(0, header.DelimitedText);
  finally
    dataset.Free;
    header.Free;
  end;
end;

procedure TForm1.FillgridData(var list: TStringList);    {addlist_data}
var
  dataset: TStringList;
  header: TStrings;
  i, maxcolumns: integer;

begin
  dataset:=TStringList.Create;
  try
    dataset.StrictDelimiter:=true;
    dataset.Delimiter:=dtsep;
    gridData.RowCount:=list.Count+1;
    maxcolumns:=MinimalBytesPerMessage;
    for i:=0 to list.Count-1 do begin
      dataset.DelimitedText:=list[i];
      dataset[0]:=SaleaeTimeToOutputTime(dataset[0]);
      if dataset.Count>maxcolumns then
        maxcolumns:=dataset.Count;
      gridData.Rows[i+1]:=dataset;
    end;
    gridData.ColCount:=maxcolumns;
    gridData.AutoSizeColumns;

    header:=gridData.Rows[0];
    header.StrictDelimiter:=true;
    header.Delimiter:=dtsep;
    list.Insert(0, header.DelimitedText);
  finally
    header.Free;
    dataset.Free;
  end;
end;

procedure TForm1.HexViewABinaryFile(fn: string);
var
  instream: TMemoryStream;

begin
  StatusBar1.Panels[1].Text:='Hex view '+fn;
  instream:=TMemoryStream.Create;
  try
    instream.LoadFromFile(fn);
    StatusBar1.Panels[0].Text:=IntToStr(instream.Size);
    StatusBar1.Panels[4].Text:=fn;
    HexViewABinaryStream(instream, gridRaw);
    pcMain.ActivePage:=tsRaw;
  finally
    instream.Free;
  end;
end;

procedure TForm1.ConvertToBinary(fn: string);
var
  inlist: TStringList;
  outputstream: TMemoryStream;
  b: byte;
  i: integer;

begin
  inlist:=TStringList.Create;
  Screen.Cursor:=crHourglass;
  Application.ProcessMessages;
  StatusBar1.Panels[1].Text:='To binary';
  outputstream:=TMemoryStream.Create;
  try
    inlist.LoadFromFile(fn);
    StatusBar1.Panels[0].Text:=IntToStr(inlist.Count-1);
    StatusBar1.Panels[4].Text:=fn;
    outputstream.SetSize(inlist.Count-1);
    outputstream.Position:=0;
    for i:=1 to inlist.Count-1 do begin
      b:=HexStrValueToInt(inlist[i].Split([sep])[1]);
      outputstream.WriteByte(b);
    end;
    HexViewABinaryStream(outputstream, gridRaw);
    SaveDialog.FileName:=ChangeFileExt(fn, '')+'_Binary.bin';
    if cbAutoSave.Checked then
      outputstream.SaveToFile(SaveDialog.FileName);
    gridRaw.AutoSizeColumns;
    pcMain.ActivePage:=tsRaw;
    gridRaw.Col:=0;
    gridRaw.Row:=1;
    gridRaw.SetFocus;
  finally
    outputstream.Free;
    inlist.Free;
    Screen.Cursor:=crDefault;
  end;
end;


{Source of recorded raw file from Saleae UART recording
 0: Invalid
 1: Other file Saleae recording
 2: YMAV FE message    >12
 3: SR24 message    >6
 4: Sensor file 0xBC
 5: Framing Error
 6: USB-Recording     (FD messages)
 7: YMAV FD message
 8: Q500 GUI
 9: CGO3 msg 88 77 .... 99
 }

function TForm1.CheckRawFileFormat(fn: string): byte;
var
  inlist: TStringList;
  i, framingerrorcounter: integer;
  byte1, byte2, byte3: byte;
  binfile: TMemoryStream;

begin
  result:=0;
  framingerrorcounter:=0;
  Inlist:=TStringList.Create;
  try
    inlist.LoadFromFile(fn);
    if Inlist.Count>0 then begin
      if pos('USBMONV', inlist[0])=1 then
        exit(6);
      if trim(inlist[0])=SaleaeHeader then
        result:=1;
      if InList.Count>MinimalBytesPerMessage then begin
        for i:=1 to Inlist.Count-MinimalBytesPerMessage do begin
          if pos(sep+sep+'Error', inlist[i])>0 then begin
            inc(framingerrorcounter);
            if framingerrorcounter>32 then
              exit(5);
          end;
          byte1:=HexStrValueToInt(inlist[i].Split([sep])[1]);
          byte2:=HexStrValueToInt(inlist[i+1].Split([sep])[1]);
          byte3:=HexStrValueToInt(inlist[i+2].Split([sep])[1]);

          if (byte1=MagicFE) and                                           {magic FE}
             (byte2<NumPayloadBytes) and                    {len}
             (HexStrValueToInt(inlist[i+4].Split([sep])[1])=0) and         {CompID}
             (HexStrValueToInt(inlist[i+6].Split([sep])[1])=0) then begin  {SubTargetID}
             CheckIfFilterIsSetMAV;
            exit(2);
          end;

          if (byte1=MagicFD) and                                           {magic FD}
             (byte2<NumPayloadBytes) and                    {len}
             (byte3=0) then begin                                          {Flags}
             CheckIfFilterIsSetMAV;
            exit(7);
          end;

          if (byte1=header1) and (byte2=header2) and                       {magic SR24}
             (byte3>6) and (byte3<maxlen) and                              {len}
             (HexStrValueToInt(inlist[i+3].Split([sep])[1]) in ValidMsgTypes) then begin
            CheckIfFilterIsSetSR24;
            exit(3);
          end;

          if (byte1=MagicFA) and (byte2=MagicFA) and                       {magic Q500 GUI}
             (byte3=1) then begin
            StatusBar1.Panels[3].Text:='No filter';
            exit(8);
          end;

          if (byte1=$88) and (byte2=$77) then begin                        {magic CGO3 at 230400 baud}
            StatusBar1.Panels[3].Text:='No filter';
            exit(9);
          end;

        end;
      end;
    end;

    if result=0 then begin            {Try if it is a binary sensor file}
      binfile:=TMemoryStream.Create;
      try
        binfile.LoadFromFile(fn);
        if binfile.Size>MinimalBytesPerMessage then begin
          binfile.Position:=0;
          repeat
            byte1:=binfile.ReadByte;
            if byte1=MagicBC then begin
              byte3:=binfile.ReadByte;      {len}
              byte1:=binfile.ReadByte;
              byte1:=binfile.ReadByte;
              byte2:=binfile.ReadByte;
              if (byte1=1) and (byte2=1) then begin
                for i:=1 to byte3+4 do
                  byte1:=binfile.ReadByte;
                if byte1=MagicBC then
                  exit(4);
              end;
            end;
          until
            (binfile.Position>512) or (binfile.Position>binfile.Size-2);
        end;
      finally
        binfile.Free;
      end;
    end;

  finally
    btnDecode.Tag:=result;                         {for further development}
    inlist.Free;
  end;
end;

function TForm1.DoFilterYMAV(msg: TMAVmessage): boolean;
var
  isMsgID, isSysID, isTargetID, isLenght: boolean;

begin
  if rgMsgID.ItemIndex=rgMsgId.Items.Count-1 then
    exit(false);
  edLength.Text:=trim(edLength.Text);
  edOther.Text:=trim(edOther.Text);
  isMsgID:=false;
  isSysID:=false;
  isTargetID:=false;
  isLenght:=(edLength.Text='') or (msg.msglength=StrToIntDef(edLength.Text, 0));
  case rgMsgID.ItemIndex of
    0: isMsgID:=true;
    1: isMsgID:=(msg.msgid32=StrToIntDef(edOther.Text, 999)) or (edOther.Text='');
  else
    isMsgID:=msg.msgid=StrToInt(rgMsgID.Items[rgMsgID.ItemIndex]);
  end;
  if cbOrOther.Checked then
    isMsgID:=isMsgID or (msg.msgid=StrToIntDef(edOther.Text, 999));
  if rgSysID.ItemIndex=0 then
    isSysID:=true
  else
    isSysID:=msg.sysid=StrToInt(rgSysID.Items[rgSysID.ItemIndex]);
  if rgTargetID.ItemIndex=0 then
    isTargetID:=true
  else
    isTargetID:=msg.TargetID=StrToInt(rgTargetID.Items[rgTargetID.ItemIndex]);
  result:=isMsgID and isSysID and isTargetID and isLenght;
end;

function TForm1.DoFilterSR24(msg: TPayload): boolean;
begin
  if rgMsgType.ItemIndex=rgMsgType.Items.Count-1 then
    exit(false);
  result:=true;
  case rgMsgType.ItemIndex of
    1: result:=(msg[3]=2) or (msg[3]=5);     {5: H920 Telemetry}
    2: result:=msg[3]=3;
    3: result:=(msg[3]=0) or (msg[3]=1);     {1: Only possible with ST24}
    4: result:=msg[3]=20;                    {This is the interesting part!}
  end;
end;

procedure TForm1.CheckIfFilterIsSetMAV;
begin
  StatusBar1.Panels[3].Text:='';
  if (rgMsgID.ItemIndex>0) or
     (rgSysID.ItemIndex>0) or
     (rgTargetID.ItemIndex>0) or
     (edLength.Text<>'') or
     ((edOther.Text<>'') and cbOrOther.Checked) then
    StatusBar1.Panels[3].Text:='Filtered';
end;

procedure TForm1.CheckIfFilterIsSetSR24;
begin
  StatusBar1.Panels[3].Text:='';
  if (rgMsgType.ItemIndex>0) then
    StatusBar1.Panels[3].Text:='Filtered';
end;

function FillMsgList(list: TStringList; checklist: TListBox; mode: byte): integer;
var
  i, v: integer;

begin
  result:=list.Count;
  if list.Count>0 then begin
    checklist.Items.Clear;
    for i:=0 to list.Count-1 do begin
      v:=StrToInt(list[i]);
      case mode of
        0: checklist.Items.Add(IntToStr(v)+msgsep+MsgIDToStr(v));
        1: checklist.Items.Add(IntToStr(v)+msgsep+MessageTypeToStr(v));
        2: checklist.Items.Add(IntToStr(v)+msgsep+SensorTypeToStr(v));
        3: checklist.Items.Add(IntToStr(v)+msgsep+ActionTypeToStr(v));
      end;
    end;
  end;
end;

function TForm1.OutputFormatByte(b: byte): string;
begin
  result:='';
  case rgOutputFormat.ItemIndex of
    0: result:=IntToStr(b);
    1: result:=IntToHex(b);
    2: result:=CharOutput(b);
  end;
end;

procedure TForm1.FilterInfo;
begin
  if gridData.RowCount<2 then
    StatusBar1.Panels[4].Text:=errFilter;
  if gridData.RowCount<gridRaw.RowCount then
    StatusBar1.Panels[3].Text:=capFilter+'!'
  else
    StatusBar1.Panels[3].Text:='';
end;

function TForm1.FilenameProposal: string;
begin
  result:='';
  case rgOutputFormat.ItemIndex of
    0: result:='_dec';
    1: result:='_hex';
    2: result:='_chr';
  end;
  result:=result+csvext;
end;

procedure TForm1.btnDelLClick(Sender: TObject);
begin
  edLength.Text:='';
end;

procedure TForm1.btnDelLMClick(Sender: TObject);
begin
  edOther.Text:='';
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  DefaultFormatSettings.DecimalSeparator:='.';
  StatusBar1.Panels[2].Text:=rgOutputFormat.Items[rgOutputFormat.ItemIndex];
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption:=AppName+tab2+AppVersion;
  acSaveRaw.Hint:=hntSaveRaw;
  acSaveData.Hint:=hntSaveData;
  acConvert.Hint:=hntConvert;
  acDecodeUART.Hint:=hntMerge;
  acResetAll.Hint:=hntResetAll;
  acReLoad.Hint:=hntReLoad;
  acClose.Hint:=hntClose;
  btnReLoad.Enabled:=false;
  randomize;
  gridRaw.AlternateColor:=clGridHighlightRows;
  Memo1.Text:='';
  CRCextra:=0;
  edCRCextra.Text:='';
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
  i: integer;

begin
  OpenDialog.Files.Clear;
  for i:=0 to high(filenames) do
    OpenDialog.Files.Add(FileNames[i]);
  OpenDialog.FileName:=FileNames[0];
  Application.BringToFront;
  DecodeUART;
end;

procedure TForm1.gbExtraToolDblClick(Sender: TObject);
begin
  edID.Text:='';
  edCRCextra.Text:='';
  edID.SetFocus;
end;

procedure TForm1.gridDataDblClick(Sender: TObject);
begin
  if gridData.Col>1 then begin
    FilterColumn(gridData, gridData.Col, gridData.Cells[gridData.Col, gridData.Row]);
  end else
    ResetAllFilterColumn(gridData);
end;

procedure TForm1.gridDataGetCellHint(Sender: TObject; ACol, ARow: Integer;
                                     var HintText: String);
begin
  HintText:=gridData.Cells[aCol, aRow];
  if (aCol>6) and (aRow>0) and (trim(gridData.Cells[aCol, aRow])<>'') then begin
    HintText:=GetDataCellInfo(aCol, aRow);
  end;
end;

procedure TForm1.gridDataHeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
begin
  gridData.Col:=0;
  ResetAllFilterColumn(gridData);
end;

procedure TForm1.gridDataPrepareCanvas(Sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
begin
  if (aCol>6) and ((((aCol+1) div 4) mod 2)=0) and (aRow>0) then
    griddata.Canvas.Brush.Color:=clGridHighlightRows;
end;

procedure TForm1.gridRawDblClick(Sender: TObject);
begin
  if (gridRaw.Col>1) and (gridRaw.Row>0) then begin
    FilterColumn(gridRaw, gridRaw.Col, gridRaw.Cells[gridRaw.Col, gridRaw.Row]);
  end else
    ResetAllFilterColumn(gridRaw);
end;

procedure TForm1.gridRawGetCellHint(Sender: TObject; ACol, ARow: Integer;
                                                    var HintText: String);
begin
  HintText:=gridRaw.Cells[aCol, aRow];
  if aRow>0 then begin
    HintText:=GetRawCellInfo(gridRaw.Cells[aCol, aRow], aCol, aRow);
  end;
end;

procedure TForm1.gridRawHeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
begin
  gridRaw.Col:=0;
  ResetAllFilterColumn(gridRaw);
end;

procedure TForm1.gridRawPrepareCanvas(Sender: TObject; aCol, aRow: Integer;
                                      aState: TGridDrawState);
begin
  if aRow>0 then begin
    case aCol of
      4: if gridRaw.Cells[1, aRow]='55' then
        gridRaw.Canvas.Brush.Color:=clMsgID;
      5: if gridRaw.Cells[1, aRow]=MagicFA_AsText then
        gridRaw.Canvas.Brush.Color:=clMsgID;
      8: if gridRaw.Cells[1, aRow]=MagicFE_AsText then
        gridRaw.Canvas.Brush.Color:=clMsgID;
      14: if (gridRaw.Cells[1, aRow]=MagicFE_AsText) and (gridRaw.Cells[8, aRow]='FF') then
        gridRaw.Canvas.Brush.Color:=clActionType;
    end;
  end;
end;

procedure TForm1.Memo1MouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then begin
    Memo1.Font.Size:=Memo1.Font.Size-1;
  end;
end;

procedure TForm1.Memo1MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then begin
    Memo1.Font.Size:=Memo1.Font.Size+1;
  end;
end;

procedure TForm1.mnDoChartLeftClick(Sender: TObject);    {Left}
begin
  DoChart(1);
end;

procedure TForm1.mnDoChartRightClick(Sender: TObject);   {Right}
begin
  DoChart(2);
end;

procedure TForm1.rgDataTypeClick(Sender: TObject);
begin
  RecreateChart(gridData);                              {Always left}
end;

function GetAxisInfo(pkt: TByteInfo): string;
begin
  result:='';
  with pkt do begin
    result:=IntToHex(MagicByte, 2)+'  Msg '+IntToStr(MsgId)+' --> byte position: '+ IntToStr(index-1);
  end;
end;

procedure TForm1.DoChart(chart: byte);        {1 left, 2 right}
var
  byteinfo, vinfo: TByteInfo;
  i: integer;
  tp: TDateTime;
  value: single;

begin
  if (gridData.Col>6) and (gridData.Row>0) then begin
    vinfo:=FillByteInfo(gridRaw, gridData.Col, gridData.Row);
    if vinfo.len>0 then begin
      case chart of
        1: begin                {Left}
             serLeft.Clear;
             Chart1.AxisList[0].Title.Caption:=GetAxisInfo(vinfo);
           end;
        2: begin                {Right}
             serRight.Clear;
             Chart1.AxisList[2].Title.Caption:=GetAxisInfo(vinfo);;
           end;
      end;
      value:=0;
      for i:=1 to gridRaw.RowCount-1 do begin
        byteinfo:=FillByteInfo(gridRaw, gridData.Col, i);
        with byteinfo do begin
          if (MagicByte=vinfo.MagicByte) and
             (MsgID=vinfo.MsgID) and
             (len=vinfo.len) then begin
            tp:=SecondsToDateTime(gridRaw.Cells[0, i]);
            case rgDataType.Itemindex of
              0: value:=AsByte;
              1: value:=GetInt16(AsByte, nextByte1);
              2: value:=GetUInt16(AsByte, nextByte1);
              3: value:=GetInt32(AsByte, nextByte1, nextByte2, nextByte3);
              4: value:=GetUInt32(AsByte, nextByte1, nextByte2, nextByte3);
              5: value:=GetFloat(AsByte, nextByte1, nextByte2, nextByte3);
            end;
            case chart of
              1: serLeft.AddXY(tp, value);
              2: serRight.AddXY(tp, value);
            end;
          end;
        end;
      end;
      StatusBar1.Panels[1].Text:=inttoStr(serLeft.Count);
      pcMain.ActivePage:=tsChart;
    end;
  end;
end;

procedure TForm1.rgOutputFormatClick(Sender: TObject);
begin
  StatusBar1.Panels[2].Text:=rgOutputFormat.Items[rgOutputFormat.ItemIndex];
end;

procedure TForm1.rgSysIDClick(Sender: TObject);
begin
  if rgSysID.ItemIndex>0 then
    lblSysID.Caption:=SysIDToStr(StrToInt(rgSysID.Items[rgSysID.ItemIndex]))
  else
    lblSysID.Caption:=capSender;
end;

procedure TForm1.rgTargetIDClick(Sender: TObject);
begin
  if rgTargetID.ItemIndex>0 then
    lblTarget.Caption:=TargetIDToStr(StrToInt(rgTargetID.Items[rgTargetID.ItemIndex]))
  else
    lblTarget.Caption:=capTarget;
end;

procedure TForm1.btnBackClick(Sender: TObject);
begin
  pcMain.ActivePage:=tsData;
end;

procedure TForm1.btnClearRedClick(Sender: TObject);
begin
   serRight.Clear;
end;


///////////////////////////// Actions ///////////////////////////////////

procedure TForm1.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TForm1.acAboutExecute(Sender: TObject);
begin
  MessageDlg(AppName+tab2+AppVersion+
             sLineBreak+sLineBreak+meinname+sLineBreak+homepage,
             mtInformation,[mbOK],0);
end;

procedure TForm1.acConvertExecute(Sender: TObject);
begin
  OpenDialog.Title:=titConvert;
  OpenDialog.Options:=OpenDialog.Options-[ofAllowMultiSelect];
  if OpenDialog.Execute then begin
    btnReLoad.Enabled:=false;
    ConvertToBinary(OpenDialog.FileName);
  end;
end;

procedure TForm1.acGoFilterExecute(Sender: TObject);
begin
  pcMain.ActivePage:=tsSettings;
end;

procedure TForm1.acResetAllExecute(Sender: TObject);
begin
  rgMsgID.ItemIndex:=0;
  rgMsgType.ItemIndex:=0;
  rgSysID.ItemIndex:=0;
  rgTargetID.ItemIndex:=0;
  edLength.Text:='';
  edOther.Text:='';
  cbOrOther.Checked:=false;
  StatusBar1.Panels[3].Text:='';
  cbCheckCRC.Checked:=false;
end;

procedure TForm1.acHexViewExecute(Sender: TObject);
begin
  OpenDialog.Title:=titHexView;
  OpenDialog.Options:=OpenDialog.Options-[ofAllowMultiSelect];
  if OpenDialog.Execute then begin
    btnReLoad.Enabled:=false;
    HexViewABinaryFile(OpenDialog.FileName);
  end;
end;

procedure TForm1.acSaveDataExecute(Sender: TObject);
begin
  SaveDialog.Title:=titSaveData;
  SaveDialog.FileName:=ChangeFileExt(OpenDialog.FileName, '')+'_Data'+FilenameProposal;
  if SaveDialog.Execute then begin
    gridData.SaveToCSVFile(SaveDialog.FileName, dtsep);
    StatusBar1.Panels[4].Text:=rsSaved+SaveDialog.FileName;
  end;
end;

procedure TForm1.acSaveRawExecute(Sender: TObject);
begin
  SaveDialog.Title:=titSaveRaw;
  if gridRaw.Cells[0, 0]=rsAdr then
    SaveDialog.FileName:=ChangeFileExt(OpenDialog.FileName, '')+'_HexView'+csvext
  else
    SaveDialog.FileName:=ChangeFileExt(OpenDialog.FileName, '')+'_Raw'+FilenameProposal;
  if SaveDialog.Execute then begin
    gridRaw.SaveToCSVFile(SaveDialog.FileName, dtsep);
    StatusBar1.Panels[4].Text:=rsSaved+SaveDialog.FileName;
  end;
end;

procedure TForm1.acProtocolExecute(Sender: TObject);
const
  separator=' - ';
  spacer='       ';

var
  i: integer;
  list: TStringList;

begin
  list:=TStringList.Create;
  try
    list.Add('Statistics');
    list.Add('==========');
    list.Add('Date/time:   '+FormatDateTime('yyyy-mm-dd hh:nn:ss', now));
    list.Add('File name:   '+OpenDialog.FileName+'   (...and maybe more)');
    if StatusBar1.Panels[5].Text<>'' then
      list.Add('System time: '+StatusBar1.Panels[5].Text);
    list.Add('');
    list.Add('');

    if lbMsgType.Items.Count>0 then begin
      list.Add('SR24 : Messages'+separator+'message counter');
      for i:=0 to lbMsgType.Items.Count-1 do
        list.Add(lbMsgType.Items[i]+separator+lbNumMsgType.Items[i]);
      if lbActionType.Items.Count>0 then begin
        list.Add(spacer+'Action types');
        for i:=0 to lbActionType.Items.Count-1 do
          list.Add(spacer+lbActionType.Items[i]);;
      end;
      list.Add('');
      list.Add('');
    end;

    if lbMsgID.Items.Count>0 then begin
      list.Add('CGO3+: Messages - message counter');
      for i:=0 to lbMsgID.Items.Count-1 do
        list.Add(lbMsgID.Items[i]+' - '+lbNumMsgID.Items[i]);
      if lbSensorType.Items.Count>0 then begin
        list.Add(spacer+'Sensor message types');
        for i:=0 to lbSensorType.Items.Count-1 do
          list.Add(spacer+lbSensorType.Items[i]);;
      end;
    end;
    list.Add('');

    SaveDialog.FileName:=ChangeFileExt(OpenDialog.FileName, '')+'_protocol.txt';
    list.SaveToFile(SaveDialog.FileName);
    StatusBar1.Panels[4].Text:=rsSaved+SaveDialog.FileName;
  finally
    list.Free;
  end;
end;

procedure TForm1.acDecodeUARTExecute(Sender: TObject);
begin
  OpenDialog.Title:=titMerge;
  OpenDialog.Options:=OpenDialog.Options+[ofAllowMultiSelect];
  if OpenDialog.Execute then begin
    DecodeUART;
  end;
end;

procedure TForm1.acReLoadExecute(Sender: TObject);
begin
  if btnReLoad.Enabled then
    DecodeUART;
end;

function ReadBCmessage(msg: TMAVmessage): uint32;  {[ms]}
begin
  result:=0;
  case msg.msgid of
//    2: result:=MAVgetUInt32(msg, 14);   {strange values, not usable}
    29, 30, 32, 33, 35, 65: result:=MAVgetUInt32(msg, 6);
  end;
end;

{Filter payload from USB traffic recorded by
 ELTIMA Software GmbH usb_analyzer trial version.
 Result is a ZIP file, unpack before to binary file started with magic USBMONV3
 https://www.softpedia.com/get/System/System-Miscellaneous/USB-Analyzer.shtml
}

procedure TForm1.DecodeBCmsg(const msg: TMAVmessage; RowNumber: integer);
var
  i: integer;

  procedure SYS_TIME;
  var
    time: TDateTime;

  begin
    lblDateTimeOut.Caption:=GetSystemTime(msg, 6, time);;
  end;

  procedure PARAM_VALUE;
  var
    i: byte;
    s: string;

  begin
    s:='';
    gridData.Cells[7, RowNumber]:=FormatFloat(ParamFloatFormat, MavGetFloatFromBuf(msg, 6));
    gridData.Cells[8, RowNumber]:='';
    gridData.Cells[9, RowNumber]:=MAV_PARAM_TYPEtoStr(msg.msgbytes[30]);
    gridData.Cells[10, RowNumber]:='';
    for i:=14 to 29 do begin
      gridData.Cells[i+1, RowNumber]:=CharOutput(msg.msgbytes[i]);
      s:=s+ASCIIOutput(msg.msgbytes[i]);
    end;
    Memo1.Lines.Add(s+' = '+gridData.Cells[7, RowNumber]);
  end;

  procedure PARAM_SET;
  var
    i: byte;
    s: string;

  begin
    s:='';
    gridData.Cells[7, RowNumber]:=FormatFloat(ParamFloatFormat, MavGetFloatFromBuf(msg, 6));
    gridData.Cells[8, RowNumber]:='';
    gridData.Cells[9, RowNumber]:=MAV_PARAM_TYPEtoStr(msg.msgbytes[28]);
    gridData.Cells[10, RowNumber]:='';
    for i:=12 to 27 do begin
      gridData.Cells[i+1, RowNumber]:=CharOutput(msg.msgbytes[i]);
      s:=s+ASCIIOutput(msg.msgbytes[i]);
    end;
    Memo1.Lines.Add(s+' = '+gridData.Cells[7, RowNumber]);
  end;

  procedure SERIAL_NUMBER;
  begin
    lblSerialNoOut.Caption:=GetSerialNumber(msg, 6);
  end;

  procedure COMMAND_LONG;
  var
    i: byte;

  begin
    for i:=6 to msg.msglength+LengthFixPartBC-1 do
      gridData.Cells[i+1, RowNumber]:=OutputFormatByte(msg.msgbytes[i]);
    gridData.Cells[35, RowNumber]:='ID = ';
    gridData.Cells[36, RowNumber]:=IntToStr(MavGetUInt16(msg, 34));
    Memo1.Lines.Add('COMMAND_LONG: '+gridData.Cells[35, RowNumber]+
                                     gridData.Cells[36, RowNumber]);
    for i:=0 to 6 do begin
      Memo1.Lines.Add(' param'+intToStr(i+1)+' = '+
                      FormatFloat('0', MavGetFloatFromBuf(msg, i*4+6)));
    end;
(*  Alternative
    for i:=0 to 6 do begin
      Memo1.Lines.Add(' param'+intToStr(i+1)+' = '+
                      IntToHex(MavGetUint32(msg, i*4+6), 8));
    end;      *)
  end;

  procedure COMMAND_ACK;
  begin
    gridData.Cells[7, RowNumber]:='ID = ';
    gridData.Cells[8, RowNumber]:=IntToStr(MAVgetUInt16(msg, 6));
    gridData.Cells[9, RowNumber]:=MAV_RESULTtoStr(msg.msgbytes[8]);
  end;

  procedure TEXT_MSG;
  var
    i: integer;
    s: string;

  begin
    s:=SeverityToStr(msg.msgbytes[6])+': ';
    for i:=7 to msg.msglength+LengthFixPartBC-1 do begin
      gridData.Cells[i+1, RowNumber]:=CharOutput(msg.msgbytes[i]);
      s:=s+ASCIIOutput(msg.msgbytes[i]);
    end;
    Memo1.Lines.Add(s);
  end;

begin
  case msg.msgid of
    2:   SYS_TIME;
    22:  PARAM_VALUE;
    23:  PARAM_SET;
    56:  SERIAL_NUMBER;
    76:  COMMAND_LONG;
    77:  COMMAND_ACK;
    253: TEXT_MSG;
  else
    for i:=6 to msg.msglength+LengthFixPartBC-1 do
      gridData.Cells[i+1, RowNumber]:=OutputFormatByte(msg.msgbytes[i]);
  end;
end;

procedure TForm1.FilterBCmessagesFromUSBrecording(fn: string);
var
  infn, SensorBytes: TMemorystream;
  sensortypelist: TStringList;
  msg: TMAVmessage;
  b: byte;
  i: integer;
  boottime, tme: uint32;

begin
  infn:=TMemoryStream.Create;
  SensorBytes:=TMemoryStream.Create;
  sensortypelist:=TStringList.Create;
  sensortypelist.Sorted:=true;
  sensortypelist.Duplicates:=dupIgnore;
  Screen.Cursor:=crHourGlass;
  lbSensorType.Items.Clear;
  FillRawGridHeader(264);
  FillDataBCheader(264);
  Application.ProcessMessages;
  gridRaw.BeginUpdate;
  gridData.BeginUpdate;
  boottime:=0;
  try
    infn.LoadFromFile(fn);
    if infn.Size>100 then begin

      Memo1.Lines.Clear;
      Memo1.Lines.Add(fn);
      Memo1.Lines.Add('');

      infn.Position:=8;
      while infn.Position<(infn.Size-LengthFixPartBC) do begin
        repeat                                     {RecordID suchen $BC}
          b:=infn.ReadByte;
        until (b=MagicBC) or (infn.Position>=infn.Size-LengthFixPartBC);
        msg.msglength:=infn.ReadByte;              {Länge Payload mit CRC}
        infn.ReadByte;
        b:=infn.ReadByte;
        if (b=$C8) or (b=1) then begin             {Valid SysID}
          infn.Position:=infn.Position-4;
          infn.ReadBuffer(msg.msgbytes, msg.msglength+LengthFixPartBC+2);  {Länge Datensatz mit FixPart und CRC}
          msg.sysid:=msg.msgbytes[3];
          msg.targetid:=msg.msgbytes[4];
          msg.msgid:=msg.msgbytes[5];
//          msg.valid:=CheckCRC16X25(msg, LengthFixPartBC);
          sensortypelist.Add(Format('%.3d', [msg.msgid]));

          tme:=ReadBCmessage(msg);
          if tme>0 then
            boottime:=tme;
          b:=LengthFixPartBC+1;                    {With CRC}
          if cbSensorFile.Checked then
            for i:=0 to msg.msglength+b do
              SensorBytes.WriteByte(msg.msgbytes[i]);

// Raw table
          if not cbRawWithCRC.Checked then
            b:=LengthFixPartBC-1;
          gridRaw.RowCount:=gridRaw.RowCount+1;
          gridData.RowCount:=gridData.RowCount+1;
          gridRaw.Cells[0, gridRaw.RowCount-1]:=FormatFloat('0.000', boottime/1000);
          gridData.Cells[0, gridData.RowCount-1]:=gridRaw.Cells[0, gridRaw.RowCount-1];
          for i:=0 to msg.msglength+b do
            gridRaw.Cells[i+1, gridRaw.RowCount-1]:=IntToHex(msg.msgbytes[i], 2);

// Data table
          gridData.Cells[1, gridData.RowCount-1]:=IntToHex(msg.msgbytes[0], 2);
          for i:=1 to 4 do
            gridData.Cells[i+1, gridData.RowCount-1]:=IntToStr(msg.msgbytes[i]);
          gridData.Cells[6, gridData.RowCount-1]:=SensorTypeToStr(msg.msgid);
          DecodeBCmsg(msg, gridData.RowCount-1);
        end;
      end;
    end;
    FillMsgList(sensortypelist, lbSensorType, 2);

    Memo1.Lines.Add('');
    Memo1.Lines.Add(trenner);
    Memo1.Lines.Add('');
    for i:=0 to lbSensorType.Items.Count-1 do
      Memo1.Lines.Add(lbSensorType.Items[i]);


    if cbSensorFile.Checked and (SensorBytes.Size>LengthFixPartBC) then begin
      SaveDialog.Title:=titSensorFile;
      SaveDialog.FileName:=ExtractFilePath(OpenDialog.Files[0])+
                           'Sensor'+PathDelim+'Sensor_00001.bin';
      if SaveDialog.Execute then
        SensorBytes.SaveToFile(SaveDialog.FileName);
    end;
  finally
    gridRaw.EndUpdate;
    gridData.EndUpdate;
    infn.Free;
    SensorBytes.Free;
    sensortypelist.Free;
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm1.FilterFDmessagesFromUSBrecording(fn: string);
var
  infn, SensorBytes: TMemorystream;
  MsgTypelist: TStringList;
  msg: TMAVmessage;
  b: byte;
  i: integer;
  boottime, tme: uint32;

begin
  infn:=TMemoryStream.Create;
  SensorBytes:=TMemoryStream.Create;
  MsgTypelist:=TStringList.Create;
  MsgTypelist.Sorted:=true;
  MsgTypelist.Duplicates:=dupIgnore;
  Screen.Cursor:=crHourGlass;
  lbSensorType.Items.Clear;
  FillRawGridHeader(264);
  FillDataBCheader(264);
  Application.ProcessMessages;
  gridRaw.BeginUpdate;
  gridData.BeginUpdate;
  boottime:=0;
  tme:=0;
  try
    infn.LoadFromFile(fn);
    if infn.Size>100 then begin

      Memo1.Lines.Clear;
      Memo1.Lines.Add(fn);
      Memo1.Lines.Add('');

      infn.Position:=8;
      while infn.Position<(infn.Size-LengthFixPartFD-2) do begin
        repeat                                     {RecordID suchen $FD}
          b:=infn.ReadByte;
        until (b=MagicFD) or (infn.Position>=infn.Size-LengthFixPartFD);
        msg.msglength:=infn.ReadByte;              {Länge Payload mit CRC}
        b:=infn.ReadByte;
        if b=0 then begin                          {Compatibility flags low}
          b:=infn.ReadByte;
          if b=0 then begin                        {Compatibility flags high}
            infn.Position:=infn.Position-4;
            infn.ReadBuffer(msg.msgbytes, msg.msglength+LengthFixPartFD+2);  {Länge Datensatz mit FixPart und CRC}
            msg.sysid:=msg.msgbytes[5];
            msg.targetid:=msg.msgbytes[6];
            msg.msgid32:=MavGetUint32(msg, 7) and $FFFFFF;
  //          msg.valid:=CheckCRC16MAV(msg, LengthFixPartFD);
            MsgTypelist.Add(Format('%.3d', [msg.msgid32]));

  //          tme:=ReadBCmessage(msg);
            if tme>0 then
              boottime:=tme;
            b:=LengthFixPartFD+1;                    {With CRC}
            if cbSensorFile.Checked then
              for i:=0 to msg.msglength+b do
                SensorBytes.WriteByte(msg.msgbytes[i]);

  // Raw table
            if not cbRawWithCRC.Checked then
              b:=LengthFixPartFD-1;
            gridRaw.RowCount:=gridRaw.RowCount+1;
            gridData.RowCount:=gridData.RowCount+1;
            gridRaw.Cells[0, gridRaw.RowCount-1]:=FormatFloat('0.000', boottime/1000);
            gridData.Cells[0, gridData.RowCount-1]:=gridRaw.Cells[0, gridRaw.RowCount-1];
            for i:=0 to msg.msglength+b do
              gridRaw.Cells[i+1, gridRaw.RowCount-1]:=IntToHex(msg.msgbytes[i], 2);
(*
  // Data table
            gridData.Cells[1, gridData.RowCount-1]:=IntToHex(msg.msgbytes[0], 2);
            for i:=1 to 4 do
              gridData.Cells[i+1, gridData.RowCount-1]:=IntToStr(msg.msgbytes[i]);
            gridData.Cells[6, gridData.RowCount-1]:=SensorTypeToStr(msg.msgid);
            DecodeBCmsg(msg, gridData.RowCount-1);    *)
          end;
        end;
      end;
    end;
    FillMsgList(MsgTypelist, lbSensorType, 2);

    Memo1.Lines.Add('');
    Memo1.Lines.Add(trenner);
    Memo1.Lines.Add('');
    for i:=0 to lbSensorType.Items.Count-1 do
      Memo1.Lines.Add(lbSensorType.Items[i]);


    if cbSensorFile.Checked and (SensorBytes.Size>LengthFixPartBC) then begin
      SaveDialog.Title:=titSensorFile;
      SaveDialog.FileName:=ExtractFilePath(OpenDialog.Files[0])+
                           'Sensor'+PathDelim+'Sensor_00001.bin';
      if SaveDialog.Execute then
        SensorBytes.SaveToFile(SaveDialog.FileName);
    end;
  finally
    gridRaw.EndUpdate;
    gridData.EndUpdate;
    infn.Free;
    SensorBytes.Free;
    MsgTypelist.Free;
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm1.DecodeUART;
var
  inlist, addlist_raw, addlist_data: TStringList;
  SensorBytes: TMemoryStream;
  msgIDlist, msgtypelist, sensortypelist, actiontypelist: TStringList;
  i, k, fileformat, msgcounter, msgsum: integer;
  maxColumns, ErrorCounterMAV, ErrorCounterSR24: integer;

  procedure CreateSensorFile(msg: TMAVmessage);
  var
    i: integer;

  begin
    for i:=LengthFixPartFE to msg.msgbytes[9]+LengthFixPartBC+LengthFixPartFE+1 do
      SensorBytes.WriteByte(msg.msgbytes[i]);
  end;

  procedure ReadOneYMAVfileFE(fn: string);
  var
    lineindex: integer;
    MAVmsg: TMAVmessage;
    tp: string;

  begin
    btnReLoad.Enabled:=true;;
    inlist.LoadFromFile(fn);
    lineindex:=1;
    StatusBar1.Panels[0].Text:=IntToStr(inlist.Count-1);
    repeat
      tp:=SetLengthTime(inlist[lineindex].Split([sep])[0]);
      if inlist[lineindex].Split([sep])[1]=MagicFE_AsText then begin
        MAVmsg:=NewYMavMessageFE(inlist, lineindex);
        if MAVmsg.valid then begin
          msgIDlist.Add(Format('%.3d', [MAVmsg.msgid]));
          if MAVmsg.msgid=255 then begin
            sensortypelist.Add(Format('%.3d', [MAVmsg.msgbytes[13]]));
            if cbSensorFile.Checked then
              CreateSensorFile(MAVmsg);
          end;

          if DoFilterYMAV(Mavmsg) then begin
            CRCextra:=CRC_EXTRA_FE;
            addlist_raw.Add(RawMessageToMergelist(MAVmsg, tp, LengthFixPartFE, cbRawWithCRC.Checked));
            addlist_data.Add(YMAVdataToMergelist_FE(MAVmsg, tp));
          end;

          if (lineindex<inlist.Count) and
             (inlist[lineindex].Split([sep])[1]<>MagicFE_AsText) then begin
            inc(ErrorCounterMAV);
//            MessageDlg('Test','Error at '+IntToStr(lineindex+1), mtError, [mbOK], 0);
          end;
        end;
      end else
        inc(lineindex);
    until lineindex>=inlist.Count;
    inlist.Clear;
  end;

  procedure ReadOneYMAVfileFD(fn: string);      {New cameras like E90}
  var
    lineindex: integer;
    MAVmsg: TMAVmessage;
    tp: string;
    i, id: integer;

  begin
    btnReLoad.Enabled:=true;;
    inlist.LoadFromFile(fn);
    lineindex:=1;
    StatusBar1.Panels[0].Text:=IntToStr(inlist.Count-1);
    repeat
      tp:=SetLengthTime(inlist[lineindex].Split([sep])[0]);
      if inlist[lineindex].Split([sep])[1]=MagicFD_AsText then begin
        MAVmsg:=NewYMavMessageFD(inlist, lineindex);
        CRCextra:=GetCRCextra(MAVmsg.msgid32);
        if cbCheckCRC.Checked then begin
          MAVmsg.valid:=false;
          If CRC16X25MAV(MAVmsg, LengthFixPartFD, 1, true, CRCextra)=
                     MAVgetuint16(MAVmsg, MAVmsg.msglength+LengthFixPartFD) then
            MAVmsg.valid:=true;
        end;

        if MAVmsg.valid then begin
          msgIDlist.Add(Format('%.3d', [MAVmsg.msgid32]));

// Find the CRC_EXTRA for Message
          if (edCRCextra.Text='') and (edID.Text<>'') then begin
            id:=StrToIntDef(edID.Text, 999999);
            if id<999999 then begin
              if MAVmsg.msgid32=id then begin
                for i:=0 to 255 do begin
                  If CRC16X25MAV(MAVmsg, LengthFixPartFD, 1, true, i)=
                     MAVgetuint16(MAVmsg, MAVmsg.msglength+LengthFixPartFD) then begin
                    edCRCextra.Text:=IntToStr(i);
                    break;
                  end;
                end;
              end;
            end;
          end;

          if DoFilterYMAV(Mavmsg) then begin
            addlist_raw.Add(RawMessageToMergelist(MAVmsg, tp, LengthFixPartFD, cbRawWithCRC.Checked));
            addlist_data.Add(YMAVdataToMergelist_FD(MAVmsg, tp));
          end;

          if (lineindex<inlist.Count) and
             (inlist[lineindex].Split([sep])[1]<>MagicFD_AsText) then begin
            inc(ErrorCounterMAV);
//            MessageDlg('Test','Error at '+IntToStr(lineindex+1), mtError, [mbOK], 0);
          end;

          if cbSensorFile.Checked then
            for i:=0 to MAVmsg.msglength+LengthFixPartFD+1 do
              SensorBytes.WriteByte(MAVmsg.msgbytes[i]);
        end;
      end else
        inc(lineindex);
    until lineindex>=inlist.Count;
    inlist.Clear;
  end;

  procedure ReadOneSR24File(fn: string);
  var
    lineindex: integer;
    SR24data: TPayload;
    tp: string;

  begin
    btnReLoad.Enabled:=true;;
    inlist.LoadFromFile(fn);
    lineindex:=1;
    StatusBar1.Panels[0].Text:=IntToStr(inlist.Count-1);
    repeat
      tp:=SetLengthTime(inlist[lineindex].Split([sep])[0]);
      if SR24UARTreadMsg(SR24data, inlist, lineindex) then begin   {Message found and buffer filled}
        msgtypelist.Add(Format('%.3d', [SR24data[3]]));
        if SR24data[3]=20 then
          actiontypelist.Add(Format('%.3d', [SR24data[4]]));

        if DoFilterSR24(SR24data) then begin
          addlist_raw.Add(SR24RawMessageToMergelist(SR24data, tp, LengthFixPartBC, cbRawWithCRC.Checked));
          addlist_data.Add(SR24dataToMergelist(SR24data, tp));
        end;
        if (lineindex<inlist.Count) and
           (HexStrValueToInt(inlist[lineindex].Split([sep])[1])<>header1) then
          inc(ErrorCounterSR24);
      end else
        inc(lineindex);
    until lineindex>=inlist.Count;
    inlist.Clear;
  end;

  procedure ReadOneQ500fileFA(fn: string);
  var
    lineindex: integer;
    MAVmsg: TMAVmessage;
    tp: string;
    b: byte;
    i: integer;
    csvline: string;

  begin
    btnReLoad.Enabled:=true;;
    inlist.LoadFromFile(fn);
    lineindex:=1;
    StatusBar1.Panels[0].Text:=IntToStr(inlist.Count-1);
    repeat
      tp:=SetLengthTime(inlist[lineindex].Split([sep])[0]);
      if inlist[lineindex].Split([sep])[1]=MagicFA_AsText then begin
        inc(lineindex);
        if inlist[lineindex].Split([sep])[1]=MagicFA_AsText then begin
          MAVmsg:=Default(TMAVmessage);
          MAVmsg.msgbytes[0]:=MagicFA;
          MAVmsg.msgbytes[1]:=MagicFA;
          inc(lineindex);
          b:=HexStrValueToInt(inlist[lineindex].Split([sep])[1]);
          MAVmsg.msgbytes[2]:=b;
          inc(lineindex);
          if b=1 then begin
            MAVmsg.msgbytes[3]:=HexStrValueToInt(inlist[lineindex].Split([sep])[1]);
            MAVmsg.msglength:=MAVmsg.msgbytes[3];
            if MAVmsg.msglength>0 then begin

              if MAVmsg.msglength=MagicFA then
                MAVmsg.msglength:=$56;                {strange behavior, try with standard length $56=86}

              for i:=4 to MAVmsg.msglength+3 do begin
                inc(lineindex);
                MAVmsg.msgbytes[i]:=HexStrValueToInt(inlist[lineindex].Split([sep])[1]);
              end;
            end;
          end else begin
            MAVmsg.msglength:=0;
          end;
        end;
        inc(lineindex);
        MAVmsg.valid:=true;

// Fill raw table
        csvline:=tp;
        if MAVmsg.msglength>0 then begin
          for i:=0 to MAVmsg.msglength+3 do
            csvline:=csvline+dtsep+IntToHex(MAVmsg.msgbytes[i], 2);

          if cbRawWithCRC.Checked then
            csvline:=csvline+dtsep+IntToHex(UnknownCRC32(MAVmsg), 8);
        end else begin
          for i:=0 to 2 do
            csvline:=csvline+dtsep+IntToHex(MAVmsg.msgbytes[i], 2);
        end;
        addlist_raw.Add(csvline);

// Fill data table
        csvline:=tp+dtsep;
        if MAVmsg.msglength>0 then begin
          csvline:=csvline+dtsep+IntToHex(MAVmsg.msgbytes[4], 2)
                   +dtsep+dtsep+IntToHex(MAVmsg.msgbytes[2], 2)+dtsep+dtsep;
          for i:=5 to MAVmsg.msglength+3 do
            csvline:=csvline+dtsep+IntToHex(MAVmsg.msgbytes[i], 2);
//          csvline:=csvline+dtsep+'='+dtsep+FormatFloat(mlfl, MavGetFloatFromBuf(MAVmsg, 5));
        end else
          csvline:=csvline+IntToHex(MAVmsg.msgbytes[2], 2);
        addlist_data.Add(csvline);
      end else
        inc(lineindex);
    until lineindex>=inlist.Count;
    inlist.Clear;
  end;

  procedure ReadOneCGO3file88(fn: string);
  var
    lineindex: integer;
    MAVmsg: TMAVmessage;
    tp: string;
    i: integer;
    csvline: string;

  begin
    btnReLoad.Enabled:=true;
    inlist.LoadFromFile(fn);
    lineindex:=1;
    StatusBar1.Panels[0].Text:=IntToStr(inlist.Count-1);
    repeat
      tp:=SetLengthTime(inlist[lineindex].Split([sep])[0]);
      if inlist[lineindex].Split([sep])[1]='0x88' then begin
        inc(lineindex);
        if inlist[lineindex].Split([sep])[1]='0x77' then begin
          MAVmsg:=Default(TMAVmessage);
          MAVmsg.msgbytes[0]:=$88;
          MAVmsg.msgbytes[1]:=$77;
          MAVmsg.msglength:=35;
          for i:=2 to MAVmsg.msglength-1 do begin
            inc(lineindex);
            MAVmsg.msgbytes[i]:=HexStrValueToInt(inlist[lineindex].Split([sep])[1]);
          end;
          inc(lineindex);
          if MAVmsg.msgbytes[34]=$99 then begin
            MAVmsg.msgid:=MAVmsg.msgbytes[2];
            MAVmsg.msgid32:=MAVmsg.msgid;
            MAVmsg.valid:=true;
          end;

  // Fill raw table
          csvline:=tp;
          for i:=0 to MAVmsg.msglength-1 do
            csvline:=csvline+dtsep+IntToHex(MAVmsg.msgbytes[i], 2);

          if cbRawWithCRC.Checked then
            csvline:=csvline+dtsep+IntToHex(GB203CRC8(MAVmsg), 2);

          if DoFilterYMAV(Mavmsg) then
            addlist_raw.Add(csvline);

  // Fill data table
          csvline:=tp+dtsep+dtsep+CGO3msgIDtoStr(MAVmsg.msgid)+dtsep+dtsep+dtsep+dtsep;
          case MAVmsg.msgid of
            254: csvline:=csvline+dtsep+TextOut(MAVmsg, 4, 29);
          else
            for i:=3 to MAVmsg.msglength-2 do
              csvline:=csvline+dtsep+IntToHex(MAVmsg.msgbytes[i], 2);
          end;

          if DoFilterYMAV(Mavmsg) then
            addlist_data.Add(csvline);
        end else
          inc(lineindex);
      end else
        inc(lineindex);
    until lineindex>=inlist.Count;
    inlist.Clear;
  end;


begin
  inlist:=TStringList.Create;
  addlist_raw:=TStringList.Create;
  addlist_data:=TStringList.Create;

  msgIDlist:=TStringList.Create;
  msgtypelist:=TStringList.Create;
  actiontypelist:=TStringList.Create;
  sensortypelist:=TStringList.Create;

  SensorBytes:=TMemoryStream.Create;
  SensorBytes.Size:=0;

  actiontypelist.Sorted:=true;
  actiontypelist.Duplicates:=dupIgnore;
  sensortypelist.Sorted:=true;
  sensortypelist.Duplicates:=dupIgnore;
  lbMsgID.Items.Clear;
  lbNumMsgID.Items.Clear;
  edSumMsgID.Text:='';
  lbMsgType.Items.Clear;
  lbNumMsgType.Items.Clear;
  edSumMsgType.Text:='';
  lbActionType.Items.Clear;
  lbSensorType.Items.Clear;
  edErrorCounterMAV.Text:='';
  edErrorCounterSR24.Text:='';
  ErrorCounterMAV:=0;
  ErrorCounterSR24:=0;

  Screen.Cursor:=crHourGlass;
  MaxColumns:=NumPayloadBytes+LengthFixPartFE+2;
  FillRawGridHeader(MaxColumns);
  FillDataMergeHeader(MaxColumns);
  Application.ProcessMessages;
  MaxColumns:=MinimalBytesPerMessage;
  StatusBar1.Panels[5].Text:='';                        {System time}

  CheckIfFilterIsSetSR24;
  CheckIfFilterIsSetMAV;

  gridRaw.BeginUpdate;
  gridData.BeginUpdate;
  try
    StatusBar1.Panels[0].Text:=IntToStr(OpenDialog.Files.Count);
    StatusBar1.Panels[4].Text:=OpenDialog.Files[0];
    if CheckRawFileFormat(OpenDialog.FileName)=6 then begin
      btnReLoad.Enabled:=false;
//      FilterBCmessagesFromUSBrecording(OpenDialog.FileName);
      FilterFDmessagesFromUSBrecording(OpenDialog.FileName);
      exit;
    end;
    for i:=0 to OpenDialog.Files.Count-1 do begin
      fileformat:=CheckRawFileFormat(OpenDialog.Files[i]);              {Find out what file format it is}
      case fileformat of
        0: InfoInvalidFile(OpenDialog.Files[i]);
        1: InfoSaleaeRecording(OpenDialog.Files[i], false);
        2: ReadOneYMAVfileFE(OpenDialog.Files[i]);                        {for merge}
        3: ReadOneSR24File(OpenDialog.Files[i]);
//        4: DecodeSensorFile(OpenDialog.Files[i]);
        7: ReadOneYMAVfileFD(OpenDialog.Files[i]);
        8: ReadOneQ500fileFA(OpenDialog.Files[i]);
        9: ReadOneCGO3file88(OpenDialog.Files[i]);
      end;
    end;

    addlist_raw.Sort;
    Application.ProcessMessages;
    addlist_data.Sort;
    Application.ProcessMessages;
    StatusBar1.Panels[0].Text:=IntToStr(addlist_raw.Count); {Anzahl der Messages aus beiden Dateien}

    FillgridRaw(addlist_raw);
    FillgridData(addlist_data);

    FillMsgList(sensortypelist, lbSensorType, 2);
    FillMsgList(actiontypelist, lbActionType, 3);

    lbNumMsgID.Items.Clear;
    edSumMsgID.Text:='';
    if msgIDlist.Count>0 then begin
      actiontypelist.Clear;
      msgsum:=0;
      for i:=0 to msgIDlist.Count-1 do
        actiontypelist.Add(msgIDlist[i]);
      FillMsgList(actiontypelist, lbMsgID, 0);
      for k:=0 to actiontypelist.Count-1 do begin
        msgcounter:=0;
        lbNumMsgID.Items.Add('');
        for i:=0 to msgIDlist.Count-1 do                    {Count messages per ID}
          if msgIDlist[i]=actiontypelist[k] then
            inc(msgcounter);
        msgsum:=msgsum+msgcounter;
        lbNumMsgID.Items[k]:=IntToStr(msgcounter);
      end;
      edSumMsgID.Text:=IntToStr(msgsum);
    end;

    lbNumMsgType.Items.Clear;
    edSumMsgType.Text:='';
    if msgTypelist.Count>0 then begin
      actiontypelist.Clear;
      msgsum:=0;
      for i:=0 to msgtypelist.Count-1 do
        actiontypelist.Add(msgtypelist[i]);
      FillMsgList(actiontypelist, lbMsgType, 1);
      for k:=0 to actiontypelist.Count-1 do begin
        msgcounter:=0;
        lbNumMsgType.Items.Add('');
        for i:=0 to msgtypelist.Count-1 do                    {Count messages per ID}
          if msgtypelist[i]=actiontypelist[k] then
            inc(msgcounter);
        msgsum:=msgsum+msgcounter;
        lbNumMsgType.Items[k]:=IntToStr(msgcounter);
      end;
      edSumMsgType.Text:=IntToStr(msgsum);
    end;
    if ErrorCounterMAV>0 then
      edErrorCounterMAV.Text:=IntToStr(ErrorCounterMAV);
    if ErrorCounterSR24>0 then
      edErrorCounterSR24.Text:=IntToStr(ErrorCounterSR24);

    if cbAutoSave.Checked then begin                       {Autosave merged lists}
      SaveDialog.FileName:=ChangeFileExt(OpenDialog.Files[0], '')+'_decoded_raw'+csvext;
      addlist_raw.SaveToFile(SaveDialog.FileName);
      SaveDialog.FileName:=ChangeFileExt(OpenDialog.Files[0], '')+'_decoded_data'+csvext;
      addlist_data.SaveToFile(SaveDialog.FileName);
    end;

    if cbSensorFile.Checked and (SensorBytes.Size>LengthFixPartBC) then begin
      SaveDialog.Title:=titSensorFile;
      SaveDialog.FileName:=ChangeFileExt(OpenDialog.Files[0], '.bin')+'_Sensor.bin';
      if SaveDialog.Execute then
        SensorBytes.SaveToFile(SaveDialog.FileName);
    end;

    gridData.AutoSizeColumns;
    gridData.Row:=1;
    griddata.Col:=2;
  finally
    gridRaw.EndUpdate;
    gridData.EndUpdate;
    inlist.Free;
    addlist_raw.Free;
    addlist_data.Free;
    msgIDlist.Free;
    msgtypelist.Free;
    sensortypelist.Free;
    actiontypelist.Free;
    SensorBytes.Free;
    Screen.Cursor:=crDefault;
  end;
end;

end.

