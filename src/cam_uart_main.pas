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
0x01	? could probably be MAV Version 1           12
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

*)

unit cam_uart_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Menus, ActnList, Grids, XMLPropStorage, Buttons, math,
  mav1defs, SR24_dec, uart_common;

Const
  AppName='CGO3+/SR24 UART';
  AppVersion='V1.2 2024-08-17';
  meinName='H. Elsner';
  homepage='http://h-elsner.mooo.com';

  sep=',';
  dtsep=';';
  msgsep=': ';
  csvext='.csv';
  tab1=' ';
  tab2='  ';

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
  rsTime='Time';
  rsLoad='Load a logic analyzer recording of UART from ';
  rsDecode=' to decode messages';
  rsSave='Save the data from the ';
  rsCSVFile=' as CSV file';

  capFilter='Filter';
  capSender='Sender';
  capTarget='Target';

  titDecode='Load file to decode...';
  titConvert='Load file to convert to binary...';
  titMerge='Select CSV files to merge...';
  titSaveRaw='Save Raw table as CSV file...';
  titSaveData='Save Data table as CSV file...';
  titHexView='Load file for hex view...';

  errInvalidFile=' is an invalid file';
  errFilter='No match found - check filter criteria!';
  errUnknownMsg=' has unknown message format -> no action';

  hntMoreFiles='Select more than one file to merge!';
  hntSaveRaw=rsSave+'"Raw" table'+rsCSVFile;
  hntSaveData=rsSave+'"Data" table'+rsCSVFile;
  hntConvert='Convert a recording from Saleae Logic Analysator in binary or text file';
  hntCreateTimestamp='Create an example for a time stamp and search it';
  hntMerge='Select files from different channels of one recording of the logic analyzer';
  hntResetAll='Clear filter criteria';
  hntClearSearch='Clear search criteria';
  hntDecodeMAV=rsLoad+'CGO3+'+rsDecode;
  hntDecodeSR24=rsLoad+'SR24'+rsDecode;
  hntDecodeUART=rsLoad+'SR24 or CGO3+'+rsDecode;
  hntClose='Close application';


  RawMergeHeader=rsTime+dtsep+rsMagic;                        {Use dtsep (;)}
  DataMergeHeader=rsTime+dtsep+rsSeqNo+dtsep+rsMsgType+dtsep+'ActionType;Sys_ID;Target_ID;RSSI[%]';

  RawSR24Header='Hdr1;Hdr2;Len;Msg_Type;Counter/Action;_?_;RSSI;PackageCtnr';
  SR24DataHeader=rsTime+dtsep+rsMsgType+dtsep+'ActionType;RSSI[%]';

  RawMavHeaderCommon=rsMagic+';Len;Seq_No;Sys_ID;Comp_ID;';
  RawMavHeaderTarget=rsTargetID+dtsep+'SubTarget_ID;Msg_ID;Msg_Type/PL1';
  YMAVDataHeader=rsTime+dtsep+rsMsgID+dtsep+rsSysID+dtsep+rsTargetID;


type

  { TForm1 }

  TForm1 = class(TForm)
    acDecodeMAV: TAction;
    acSaveRaw: TAction;
    acSaveData: TAction;
    acClose: TAction;
    acConvert: TAction;
    acMerge: TAction;
    acAbout: TAction;
    acInfo: TAction;
    acResetAll: TAction;
    acCreateTimestamp: TAction;
    acClearSearch: TAction;
    acDecodeSR24: TAction;
    acDecodeUART: TAction;
    acHexView: TAction;
    acByteConvert: TAction;
    ActionList1: TActionList;
    btnByteConvert: TBitBtn;
    btnConvert: TButton;
    btnDecodeMAV: TButton;
    btnClose: TButton;
    btnDelLM: TButton;
    btnMerge: TButton;
    btnResetAll: TButton;
    btnSaveRaw: TButton;
    btnSaveData: TButton;
    btnDelL: TButton;
    btnCreateTimestamp: TButton;
    btnClearSearch: TButton;
    cbOrOther: TCheckBox;
    cbSaveMerged: TCheckBox;
    edIntValue: TEdit;
    edByteConvert: TEdit;
    edLength: TEdit;
    edOther: TEdit;
    gbFilter: TGroupBox;
    gbSR24Filter: TGroupBox;
    gbOtherMsgID: TGroupBox;
    lblActionType: TLabel;
    lblMsgType: TLabel;
    lblSensorType: TLabel;
    lblMsgID: TLabel;
    lblSearchbytes: TLabel;
    lblTarget: TLabel;
    lblSysID: TLabel;
    lblLenght: TLabel;
    lbMsgID: TListBox;
    lbMsgType: TListBox;
    lbActionType: TListBox;
    lbSensorType: TListBox;
    MainMenu1: TMainMenu;
    rgByteConvert: TRadioGroup;
    Separator4: TMenuItem;
    mnHexView: TMenuItem;
    Separator3: TMenuItem;
    mnResetFilter: TMenuItem;
    mnGoFilter: TMenuItem;
    rgMsgType: TRadioGroup;
    rgTimestamp: TRadioGroup;
    Separator1: TMenuItem;
    mnAbout: TMenuItem;
    mnInfo: TMenuItem;
    mnMerge: TMenuItem;
    mnConvert: TMenuItem;
    mnDecode: TMenuItem;
    mnTools: TMenuItem;
    mnClose: TMenuItem;
    rgTargetID: TRadioGroup;
    rgSysID: TRadioGroup;
    rgMsgID: TRadioGroup;
    Separator2: TMenuItem;
    mnSaveData: TMenuItem;
    mnSaveRaw: TMenuItem;
    mnFile: TMenuItem;
    OpenDialog: TOpenDialog;
    PageControl1: TPageControl;
    panButtons: TPanel;
    rgMsgFormat: TRadioGroup;
    rgOutputFormat: TRadioGroup;
    SaveDialog: TSaveDialog;
    gridRaw: TStringGrid;
    gridData: TStringGrid;
    StatusBar1: TStatusBar;
    gridSearchBytes: TStringGrid;
    tsSearch: TTabSheet;
    tsSettings: TTabSheet;
    tsData: TTabSheet;
    tsRaw: TTabSheet;
    XMLPropStorage1: TXMLPropStorage;
    procedure acAboutExecute(Sender: TObject);
    procedure acByteConvertExecute(Sender: TObject);
    procedure acClearSearchExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure acConvertExecute(Sender: TObject);
    procedure acCreateTimestampExecute(Sender: TObject);
    procedure acDecodeMAVExecute(Sender: TObject);
    procedure acDecodeSR24Execute(Sender: TObject);
    procedure acDecodeUARTExecute(Sender: TObject);
    procedure acHexViewExecute(Sender: TObject);
    procedure acMergeExecute(Sender: TObject);
    procedure acResetAllExecute(Sender: TObject);
    procedure acSaveDataExecute(Sender: TObject);
    procedure acSaveRawExecute(Sender: TObject);
    procedure btnDelLClick(Sender: TObject);
    procedure btnDelLMClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mnGoFilterClick(Sender: TObject);
    procedure rgMsgFormatClick(Sender: TObject);
    procedure rgOutputFormatClick(Sender: TObject);
    procedure rgSysIDClick(Sender: TObject);
    procedure rgTargetIDClick(Sender: TObject);
  private
    function  CheckRawFileFormat(fn: string): byte;
    procedure DecodeRecordedCSVfromCGO3(fn: string);
    procedure DecodeRecordedCSVfromSR24(fn: string);
    procedure FillRawSR24Header(const len: integer=maxlen);
    procedure FillSR24DataHeader(len: integer);
    procedure FillRawMAVHeader(len: integer);
    procedure FillMAVDataHeader(headerstring: string; len: integer);
    procedure FillRawMergeHeader(len: integer);
    procedure FillDataMergeHeader(len: integer);
    function  FilenameProposal: string;
    procedure WriteMsgToRaw(MAVmsg: TMAVmessage; msgCounter: integer);
    procedure WriteMsgToData(MAVmsg: TMAVmessage);
    function  DoFilterYMAV(msg: TMAVmessage): boolean;
    function  DoFilterSR24(msg: TPayload): boolean;
    procedure FilterInfo;
    procedure DisplaySR24RawData(RawData: TPayLoad);
    function  DisplaySR24RawDataAsCSV(RawData: TPayLoad; timept: string): string;
    procedure DisplaySR24Data(RawData: TPayLoad; timept: string);
    function  OutputFormatByte(b: byte): string;
    procedure CheckIfFilterIsSetMAV;
    procedure CheckIfFilterIsSetSR24;
    procedure ConvertToBinary(fn: string);
    procedure HexViewABinaryFile(fn: string);
    procedure DecodeSensorFile(fn: string);           {for further development}
  public

  end;

var
  Form1: TForm1;

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
  z:=0;                                                 {Counter for unsynced bytes}
  buf:=empty;                                           {Reset buffer}
  repeat
    buf[0]:=UARTreadByteFromText;                       {read byte by byte}
    if (buf[2]=header1) and                             {check if valid message (header+plausible length)}
       (buf[1]=header2) and
       (buf[0]<maxlen) and
       (buf[0]>0) then begin
      data[0]:=buf[2];                                  {Copy header and length to message data}
      data[1]:=buf[1];
      data[2]:=buf[0];
      for i:=3 to buf[0]+2 do                           {Read the other bytes of the dataset (payload + CRC)}
        data[i]:=UARTreadByteFromText;
      z:=0;
      result:=true;                                     {Message is valid but CRC was not checked (no need at this time)}
    end else begin                                      {Shift buffer right}
      buf[2]:=buf[1];
      buf[1]:=buf[0];
      inc(z);                                           {Count bytes to prevent overflow}
    end;
  until result or                                       {Valid message but w/o CRC check}
       (z>maxlen);                                      {Too long message}
end;

procedure CommonFixPartMessage(var MAVmsgFixPart: TMAVmessage; list: TStringlist; var lineidx: integer);
begin
  MAVmsgFixPart.timepoint:=SecondsToDateTime(list[lineidx].Split([sep])[0]);
  MAVmsgFixPart.msgbytes[0]:=v1magicbyte;
  inc(lineIdx);
  MAVmsgFixPart.msgLength:=HexStrValueToInt(list[lineidx].Split([sep])[1]);
  MAVmsgFixPart.msgbytes[1]:=MAVmsgFixPart.msgLength;
  inc(lineIdx);
  MAVmsgFixPart.msgbytes[2]:=HexStrValueToInt(list[lineidx].Split([sep])[1]);
  inc(lineIdx);
  MAVmsgFixPart.sysid:=HexStrValueToInt(list[lineidx].Split([sep])[1]);
  MAVmsgFixPart.msgbytes[3]:=MAVmsgFixPart.sysid;
  inc(lineIdx);
  MAVmsgFixPart.msgbytes[4]:=HexStrValueToInt(list[lineidx].Split([sep])[1]);
  inc(lineIdx);
end;

function NewYMavMessage(var list: TStringList; var lineidx: integer): TMAVmessage;
var
  i: integer;

begin
  result:=ClearMAVmessage;
  if lineidx<list.Count-8 then begin
    CommonFixPartMessage(result, list, lineidx);
    result.TargetID:=HexStrValueToInt(list[lineidx].Split([sep])[1]);
    result.msgbytes[5]:=result.TargetID;
    inc(lineIdx);
    result.msgbytes[6]:=HexStrValueToInt(list[lineidx].Split([sep])[1]);
    inc(lineIdx);
    result.msgID:=HexStrValueToInt(list[lineidx].Split([sep])[1]);
    result.msgbytes[7]:=result.msgID;
    if lineidx+result.msgLength<list.Count then begin
      for i:=8 to result.msgLength+9 do begin
        inc(lineIdx);
        result.msgbytes[i]:=HexStrValueToInt(list[lineidx].Split([sep])[1]);
      end;
      result.valid:=true;
    end;
  end;
  if result.timepoint=0 then
    result.valid:=false;
  inc(lineIdx);                                            {Next message byte}
end;

// ToDo for further development
function NewV1MavMessage(var list: TStringList; var lineidx: integer): TMAVmessage;
var
  i: integer;

begin
  result:=ClearMAVmessage;
  if lineidx<list.Count-6 then begin
    CommonFixPartMessage(result, list, lineidx);
    result.msgID:=HexStrValueToInt(list[lineidx].Split([sep])[1]);
    result.msgbytes[5]:=result.msgID;
    if lineidx+result.msgLength<list.Count then begin
      for i:=6 to result.msgLength+9 do begin
        inc(lineIdx);
        result.msgbytes[i]:=HexStrValueToInt(list[lineidx].Split([sep])[1]);
      end;
      result.valid:=true;
    end;
  end;
  if result.timepoint=0 then
    result.valid:=false;
  inc(lineIdx);                                            {Next message byte}
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
    FillBinaryHeader(grid);
    aCol:=1;
    aRow:=1;
    grid.Cells[0, 1]:='00000000';
    grid.RowCount:=1;                                   {Löschen der vorherigen Anzeigen}
    grid.RowCount:=(BinaryStream.Size div 16) +2;
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

procedure TForm1.FillRawMergeHeader(len: integer);
var
  pos: integer;

begin
  gridRaw.RowCount:=1;
  gridRaw.ColCount:=len;
  pos:=FillFixpartOfHeader(gridRaw, RawMergeHeader, 0);
  NumberingColumns(gridRaw, 'Byte', pos, len-pos);
  gridRaw.AutoSizeColumns;
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
    PageControl1.ActivePage:=tsRaw;
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
    outputstream.SaveToFile(ChangeFileExt(fn, '')+'_Binary.bin');
    gridRaw.AutoSizeColumns;
  finally
    outputstream.Free;
    inlist.Free;
    Screen.Cursor:=crDefault;
  end;
end;


{Source of recorded raw file from Saleae UART recording
 0: Invalid
 1: Other file Saleae recording
 2: YMAV message    >12
 3: SR24 message    >6
 4: Sensor file 0xBC
 }

function TForm1.CheckRawFileFormat(fn: string): byte;
var
  inlist: TStringList;
  i: integer;
  byte1, byte2, byte3: byte;
  binfile: TMemoryStream;

begin
  result:=0;
  Inlist:=TStringList.Create;
  try
    inlist.LoadFromFile(fn);
    if Inlist.Count>0 then begin
      if trim(inlist[0])=SaleaeHeader then
        result:=1;
      if InList.Count>MinimalBytesPerMessage then begin
        for i:=1 to Inlist.Count-MinimalBytesPerMessage do begin
          byte1:=HexStrValueToInt(inlist[i].Split([sep])[1]);
          byte2:=HexStrValueToInt(inlist[i+1].Split([sep])[1]);
          byte3:=HexStrValueToInt(inlist[i+2].Split([sep])[1]);

          if (byte1=v1magicbyte) and                                       {magic}
             (byte2>12) and (byte2<NumPayloadBytes) and                    {len}
             (HexStrValueToInt(inlist[i+4].Split([sep])[1])=0) and         {CompID}
             (HexStrValueToInt(inlist[i+6].Split([sep])[1])=0) then begin  {SubTargetID}
            rgMsgFormat.ItemIndex:=1;
            CheckIfFilterIsSetMAV;
            exit(2);
          end;

          if (byte1=header1) and (byte2=header2) and                       {magic}
             (byte3>6) and (byte3<maxlen) and                              {len}
             (HexStrValueToInt(inlist[i+4].Split([sep])[1]) in ValidMsgTypes) then begin
            rgMsgFormat.ItemIndex:=2;
            CheckIfFilterIsSetSR24;
            exit(3);
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
            if byte1=BCmagic then begin
              byte3:=binfile.ReadByte;      {len}
              byte1:=binfile.ReadByte;
              byte1:=binfile.ReadByte;
              byte2:=binfile.ReadByte;
              if (byte1=1) and (byte2=1) then begin
                for i:=1 to byte3+4 do
                  byte1:=binfile.ReadByte;
                if byte1=BCmagic then
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
    inlist.Free;
  end;
end;

procedure TForm1.DecodeSensorFile(fn: string);     {for further development}
begin
  StatusBar1.Panels[4].Text:=fn+' is a Sensor file from H480 flight log';

  {ToDo}
end;

function TForm1.DoFilterYMAV(msg: TMAVmessage): boolean;
var
  isMsgID, isSysID, isTargetID, isLenght: boolean;

begin
  edLength.Text:=trim(edLength.Text);
  edOther.Text:=trim(edOther.Text);
  isMsgID:=false;
  isSysID:=false;
  isTargetID:=false;
  isLenght:=(edLength.Text='') or (msg.msglength=StrToIntDef(edLength.Text, 0));
  case rgMsgID.ItemIndex of
    0: isMsgID:=true;
    1: isMsgID:=(msg.msgid=StrToIntDef(edOther.Text, 999)) or (edOther.Text='');
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
  result:=true;
  case rgMsgType.ItemIndex of
    1: result:=msg[3]=2;
    2: result:=msg[3]=3;
    3: result:=(msg[3]=0) or (msg[3]=1);                   {Only H920 + ST24}
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

procedure TForm1.WriteMsgToRaw(MAVmsg: TMAVmessage; msgCounter: integer);
var
  i, offset: integer;

begin
  gridRaw.RowCount:=msgCounter+1;
  offset:=2;

  if rgMsgFormat.ItemIndex=0 then
    offset:=0;
  gridRaw.Cells[0, msgCounter]:=IntToHex(MAVmsg.msgbytes[0], 2);
  for i:=1 to 5+offset do
    gridRaw.Cells[i, msgCounter]:=IntToStr(MAVmsg.msgbytes[i]);

  for i:=6+offset to MAVmsg.msglength+7 do begin           {+7: ohne CRC, +9: mit CRC}
    gridRaw.Cells[i, msgCounter]:=OutputFormatByte(MAVmsg.msgbytes[i]);
  end;
end;

procedure TForm1.WriteMsgToData(MAVmsg: TMAVmessage);
var
  lineidx: integer;

  function GetIntFromBuf(const pos, numbytes: integer): uint64; {Position/Anzahl Bytes}
  var
    i: integer;

  begin
    result:=0;
    for i:=0 to numbytes-1 do begin
      result:=result+MAVmsg.msgbytes[i+pos]*(256**i);
    end;
  end;

(*
  function ReadInt16(pos_low_byte: integer): uint16;
  begin
    result:=MAVmsg.msgbytes[pos_low_byte]+MAVmsg.msgbytes[pos_low_byte+1]*256;
  end;

  function ReadInt32(pos_low_byte: integer): uint32;
  begin
    result:=MAVmsg.msgbytes[pos_low_byte]+MAVmsg.msgbytes[pos_low_byte+1]*256+
            MAVmsg.msgbytes[pos_low_byte+2]*65536+
            MAVmsg.msgbytes[pos_low_byte+3]*16777216;
  end;  *)

 {https://www.delphipraxis.net/122021-single-byte-array-konvertieren-und-umgekehrt.html
 http://forum.lazarus-ide.org/index.php?topic=42182.0
 https://www.lazarusforum.de/viewtopic.php?f=55&t=14659
 Direkter Typecast mit dem Zieldatentyp oder die Deklaration mittels absolute}

  function GetFloatFromBuf(const pos: integer): single; {Position, Länge immer 4}
  var i: integer;
      wfl: packed array[0..3] of Byte;
      wx: Single absolute wfl;

  begin
    result:=0;
    for i:=0 to 3 do                               {Endianess prüfen (to/downto)}
      wfl[i]:=MAVmsg.msgbytes[i+pos];                {4 byte aus Buffer ausschneiden}
    result:=wx;                                    {Typecast mittels absolute}
  end;

(********************************* Messages ***********************************)
  procedure UnknownMessage(p: integer);
  var
    i: integer;

  begin
    for i:=p to MAVmsg.msglength+7 do begin
      gridData.Cells[i-p+4, lineidx]:=OutputFormatByte(MAVmsg.msgbytes[i]);
    end;
  end;

  procedure ParamRequest;
  var
    i: integer;
    s: string;

  begin
    gridData.Cells[4, lineidx]:=IntToHex(MAVmsg.msgbytes[8], 2);
    gridData.Cells[5, lineidx]:=IntToHex(MAVmsg.msgbytes[9], 2);
    s:='';
    for i:=10 to 21 do begin
      s:=s+CharOutput(MAVmsg.msgbytes[i]);
    end;
    gridData.Cells[6, lineidx]:=s;
    for i:=0 to 5 do begin
      gridData.Cells[7+i, lineidx]:=IntToHex(MAVmsg.msgbytes[i+22], 2);
    end;
  end;

  procedure Attitude;
  begin
    if MAVmsg.msglength=3 then begin
      gridData.Cells[4, lineidx]:='Request to '+IntToStr(MAVmsg.msgbytes[8]);
      gridData.Cells[5, lineidx]:=TargetIDToStr(MAVmsg.msgbytes[8]);
    end;
    if MAVmsg.msglength=26 then begin
      gridData.Cells[4, lineidx]:='Drone Yaw=';
      gridData.Cells[5, lineidx]:=IntToYaw(GetIntFromBuf(8, 2));
      gridData.Cells[6, lineidx]:='Drone Pitch=';
      gridData.Cells[7, lineidx]:=IntToAngle(GetIntFromBuf(10, 2));
      gridData.Cells[8, lineidx]:='Drone Roll=';
      gridData.Cells[9, lineidx]:=IntToAngle(GetIntFromBuf(12, 2));

      gridData.Cells[11, lineidx]:='tbd_14=';
      gridData.Cells[12, lineidx]:=IntToStr(GetIntFromBuf(14, 2));
      gridData.Cells[13, lineidx]:='tbd_16';
      gridData.Cells[14, lineidx]:=IntToStr(GetIntFromBuf(16, 2));
      gridData.Cells[15, lineidx]:='tbd_18=';
      gridData.Cells[16, lineidx]:=IntToStr(GetIntFromBuf(18, 2));
      gridData.Cells[17, lineidx]:='tbd_20=';
      gridData.Cells[18, lineidx]:=IntToStr(GetIntFromBuf(20, 2));

      gridData.Cells[20, lineidx]:='Camera Pan=';
      gridData.Cells[21, lineidx]:=IntToPan(GetIntFromBuf(22, 2));
      gridData.Cells[22, lineidx]:='Camera Tilt [°]=';
      gridData.Cells[23, lineidx]:=IntToTilt(GetIntFromBuf(24, 2));
      gridData.Cells[24, lineidx]:='tbd24=';    {immer neutral (2048)}
      gridData.Cells[25, lineidx]:=IntToStr(GetIntFromBuf(26, 2));
      gridData.Cells[26, lineidx]:='Camera pan mode=';
      gridData.Cells[27, lineidx]:=IntToPanMode(GetIntFromBuf(28, 2));
      gridData.Cells[28, lineidx]:='Camera tilt mode';
      gridData.Cells[29, lineidx]:=IntToTiltMode(GetIntFromBuf(30, 2));
      gridData.Cells[30, lineidx]:='tbd_30=';
      gridData.Cells[31, lineidx]:=IntToStr(GetIntFromBuf(32, 2));
    end;
  end;

  procedure Position;
  begin
    if MAVmsg.msglength=34 then begin
      gridData.Cells[4, lineidx]:='Drone Lat=';
      gridData.Cells[5, lineidx]:=IntToLatLon(GetIntFromBuf(8, 4));
      gridData.Cells[6, lineidx]:='Drone Lon=';
      gridData.Cells[7, lineidx]:=IntToLatLon(GetIntFromBuf(12, 4));
      gridData.Cells[8, lineidx]:='Drone Alt?=';
      gridData.Cells[9, lineidx]:=IntToAlt(GetIntFromBuf(16, 4));   {Alt ?}

      gridData.Cells[11, lineidx]:='RC Lat?=';
      gridData.Cells[12, lineidx]:=IntToStr(GetIntFromBuf(20,4));
      gridData.Cells[13, lineidx]:='RC Lon?=';
      gridData.Cells[14, lineidx]:=IntToStr(GetIntFromBuf(24, 4));
      gridData.Cells[15, lineidx]:='tbd_28=';
      gridData.Cells[16, lineidx]:=IntToStr(GetIntFromBuf(28, 4));

    end;
  end;

  procedure RCdata;     {Msg_ID 8, msg_lenght=38 from 4 to 1 (RC to Autopilot)}
  var
    ch_index, ch_value, offset: integer;

  begin
    gridData.Cells[4, lineidx]:='RC Lat=';
    gridData.Cells[5, lineidx]:=IntToLatLon(GetIntFromBuf(8, 4));
    gridData.Cells[6, lineidx]:='RC Lon=';
    gridData.Cells[7, lineidx]:=IntToLatLon(GetIntFromBuf(12, 4));
    gridData.Cells[8, lineidx]:='RC Alt=';       {Altitude msl, 4 bytes float}
    gridData.Cells[9, lineidx]:=FloatToAlt(GetFloatFromBuf(16));
    gridData.Cells[10, lineidx]:='Accuracy=';
    gridData.Cells[11, lineidx]:=IntToAccuracy(GetIntFromBuf(20, 2));
    gridData.Cells[12, lineidx]:='Speed=';
    gridData.Cells[13, lineidx]:=IntToSpeed(GetIntFromBuf(22, 2));
    gridData.Cells[14, lineidx]:='Angle=';
    gridData.Cells[15, lineidx]:=IntToRCAngle(GetIntFromBuf(24, 2));
    gridData.Cells[16, lineidx]:=rsSeqNo+'=';
    gridData.Cells[17, lineidx]:=IntToStr(MAVmsg.msgbytes[26]);

    ch_value:=0;
    offset:=0;
    for ch_index:=0 to 11 do begin               {Decode 12 channels from RC}
      gridData.Cells[ch_index*2+18, lineidx]:='CH'+IntToStr(ch_index+1);
      if (ch_index mod 2)=0 then begin           {gerade Channel index}
        ch_value:=(MAVmsg.msgbytes[27+ch_index+offset]*16) +
                  ((MAVmsg.msgbytes[28+ch_index+offset] shr 4) and $0F);
        inc(offset);
      end else begin                             {ungerade Ch_index}
        ch_value:=((MAVmsg.msgbytes[26+ch_index+offset] and $0F)*256) +
                   MAVmsg.msgbytes[27+ch_index+offset];
      end;
      gridData.Cells[ch_index*2+19, lineidx]:=IntToStr(ch_value);
    end;
    gridData.Cells[42, lineidx]:='GPSAccV?=';    {korreliert ganz gut mit Accuracy}
    gridData.Cells[43, lineidx]:=IntToStr(MAVmsg.msgbytes[45]);
  end;

  procedure msg_3;
  begin
    gridData.Cells[4, lineidx]:='Int16 - 1=';
    gridData.Cells[5, lineidx]:=IntToStr(GetIntFromBuf(8, 2));
    gridData.Cells[6, lineidx]:='Int16 - 2=';
    gridData.Cells[7, lineidx]:=IntToStr(GetIntFromBuf(10, 2));
    gridData.Cells[8, lineidx]:='Int16 - 3=';
    gridData.Cells[9, lineidx]:=IntToStr(GetIntFromBuf(12, 2));
  end;

  procedure msg_76;
  begin
    UnknownMessage(8);
  end;

  procedure SensorData;
  var
    i: integer;

  begin
    if MAVmsg.targetid=3 then begin
      gridData.Cells[4, lineidx]:=rsSensor+tab1+rsMagic+' 0xBC';
      gridData.Cells[5, lineidx]:=rsSensor+' len='+IntToStr(MAVmsg.msgbytes[9]);
      gridData.Cells[6, lineidx]:=rsSeqNo+'='+IntToStr(MAVmsg.msgbytes[10]);
      gridData.Cells[7, lineidx]:=rsSysID+' and Comp_ID=1';
      gridData.Cells[8, lineidx]:=rsMsgType+'=';
      gridData.Cells[9, lineidx]:=IntToStr(MAVmsg.msgbytes[13]);
      for i:=14 to MAVmsg.msglength+5 do begin
        gridData.Cells[i-4, lineidx]:=OutputFormatByte(MAVmsg.msgbytes[i]);
      end;

    end else
      UnknownMessage(8);
  end;

(******************************************************************************)

begin
  lineidx:=gridData.RowCount;
  gridData.RowCount:=lineidx+1;
  gridData.Cells[0, lineidx]:=FormatDateTime(msgtpformat, mavmsg.timepoint, [fdoInterval]);
  gridData.Cells[1, lineidx]:=msgidToStr(mavmsg.msgid);
  gridData.Cells[2, lineidx]:=sysidToStr(mavmsg.sysid);
  gridData.Cells[3, lineidx]:=targetidToStr(mavmsg.targetid);
  if rgMsgFormat.ItemIndex=0 then begin
    UnknownMessage(6);
  end else begin
    case MAVmsg.msgid of
      1: Attitude;
      2: Position;
      3: msg_3;
      8: RCdata;
      20: ParamRequest;
      76: msg_76;
      255: SensorData;
    else
      UnknownMessage(8);
    end;
  end;
end;

procedure TForm1.DecodeRecordedCSVfromSR24(fn: string);
var
  inlist: TStringList;
  lineindex: integer;
  SR24data: TPayLoad;
  tp: string;

begin
  StatusBar1.Panels[4].Text:=fn;
  inlist:=TStringList.Create;
  inlist.LoadFromFile(fn);
  if inlist.Count<MinimalBytesPerMessage then
    exit;
  StatusBar1.Panels[0].Text:=IntToStr(inlist.Count-1);
  Screen.Cursor:=crHourGlass;
  FillRawSR24Header(maxlen);
  FillSR24DataHeader(maxlen);
  Application.ProcessMessages;

  gridRaw.BeginUpdate;
  gridData.BeginUpdate;
  lineindex:=1;
  try
    repeat
      tp:=SaleaeTimeToOutputTime(inlist[lineindex].Split([sep])[0]);
      if SR24UARTreadMsg(SR24data, inlist, lineindex) then begin   {Message found and buffer filled}
        if DoFilterSR24(SR24data) then begin
          DisplaySR24RawData(SR24data);
          DisplaySR24Data(SR24data, tp);
        end;
      end else
        inc(lineindex);
    until lineindex>=inlist.Count;
    gridData.AutoSizeColumns;
    gridRaw.AutoSizeColumn(0);
    FilterInfo;
    PageControl1.ActivePage:=tsRaw;
  finally
    gridRaw.EndUpdate;
    gridData.EndUpdate;
    inlist.Free;
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm1.DisplaySR24RawData(RawData: TPayLoad);
var
  i: integer;

begin
  gridRaw.RowCount:=gridRaw.RowCount+1;
  gridRaw.Cells[0, gridRaw.RowCount-1]:=IntToHex(RawData[0], 2);
  gridRaw.Cells[1, gridRaw.RowCount-1]:=IntToHex(RawData[1], 2);
// Byte[2]: Length for all bytes except Header (55, 55) and length byte itself including CRC
  gridRaw.Cells[2, gridRaw.RowCount-1]:=IntToStr(RawData[2]);
  //  for i:=1 to RawData[2] do    {mit CRC}
  for i:=1 to RawData[2]-1 do      {ohne CRC}
    gridRaw.Cells[i+2, gridRaw.RowCount-1]:=OutputFormatByte(RawData[i+2]);
end;

function TForm1.DisplaySR24RawDataAsCSV(RawData: TPayLoad; timept: string): string;
var
  i: integer;

begin
  result:=timept+dtsep+IntToHex(RawData[0], 2)+
                 dtsep+IntToHex(RawData[1], 2)+
                 dtsep+IntToStr(RawData[2]);       {Message lenght}
  for i:=1 to RawData[2]-1 do                      {ohne CRC}
    result:=result+dtsep+OutputFormatByte(RawData[i+2]);
end;

procedure TForm1.DisplaySR24Data(RawData: TPayLoad; timept: string);
var
  i: integer;

  procedure Message0;
  var
    i: integer;

  begin
    gridData.Cells[3, gridData.RowCount-1]:=IntToStr(GetRSSI(RawData));
    for i:=6 to RawData[2]-1 do
      gridData.Cells[i-2, gridData.RowCount-1]:=OutputFormatByte(RawData[i+2]);
  end;

  procedure Message2;                {Telemetry from flight controller}
  var
    i: integer;

  begin
    for i:=4 to RawData[2]-1 do
      gridData.Cells[i, gridData.RowCount-1]:=OutputFormatByte(RawData[i+2]);
  end;

  procedure Message3;
  var
    i: integer;

  begin
    gridData.Cells[3, gridData.RowCount-1]:=IntToStr(GetRSSI(RawData));
    for i:=6 to RawData[2]-1 do
      gridData.Cells[i-2, gridData.RowCount-1]:=OutputFormatByte(RawData[i+2]);
  end;

  procedure SonarSwitch;
  begin
    gridData.Cells[4, gridData.RowCount-1]:='Sonar '+SwitchPos(RawData[5]);
  end;

  procedure LEDSwitch;
  begin
    gridData.Cells[4, gridData.RowCount-1]:='LED '+SwitchPos(RawData[5]);
  end;

  procedure HomeAltitudeSet;
  var
    home_alt: integer;

  begin
    home_alt:=GetIntFromBuf(RawData, 5, 2);
    gridData.Cells[4, gridData.RowCount-1]:=IntToStr(home_alt div 100)+'m';
    gridData.Cells[5, gridData.RowCount-1]:=OutputFormatByte(RawData[7]);
    gridData.Cells[6, gridData.RowCount-1]:=OutputFormatByte(RawData[8]);
  end;

  procedure Message20;
  var
    i: integer;

  begin
    gridData.Cells[2, gridData.RowCount-1]:=ActionTypeToStr(RawData[4]);
    case RawData[4] of                   {Action Type}
      5: SonarSwitch;
      9: LEDSwitch;
      11: HomeAltitudeSet;
      else
        for i:=3 to RawData[2]-1 do
          gridData.Cells[i+1, gridData.RowCount-1]:=OutputFormatByte(RawData[i+2]);
    end;
  end;

begin
  gridData.RowCount:=gridData.RowCount+1;
  gridData.Cells[0, gridData.RowCount-1]:=timept;
  gridData.Cells[1, gridData.RowCount-1]:=MessageTypeToStr(RawData[3]);
  case Rawdata[3] of
    0:  Message0;                      {Channel data (12 ch)}
    2:  Message2;                      {Telemetry from flight controller}
    3:  Message3;                      {Controller-GPS + channel data}
    20: Message20;                     {Additional data}
    else
      for i:=6 to RawData[2]-1 do      {ohne CRC}
        gridData.Cells[i-2, gridData.RowCount-1]:=OutputFormatByte(RawData[i+2]);
  end;
end;

procedure TForm1.DecodeRecordedCSVfromCGO3(fn: string);
var
  inlist: TStringList;
  lineindex, msgCounter: integer;
  YMAVmsg: TMAVmessage;

begin
  StatusBar1.Panels[4].Text:=fn;
  inlist:=TStringList.Create;
  inlist.LoadFromFile(fn);
  if inlist.Count<MinimalBytesPerMessage then
    exit;
  StatusBar1.Panels[0].Text:=IntToStr(inlist.Count-1);
  Screen.Cursor:=crHourGlass;
  FillRawMAVHeader(NumPayloadBytes+lenfix);
  FillMAVDataHeader(YMAVDataHeader, NumPayloadBytes+lenfix);
  Application.ProcessMessages;

  gridRaw.BeginUpdate;
  gridData.BeginUpdate;
  MsgCounter:=0;
  lineindex:=1;
  try
    repeat
      if inlist[lineindex].Split([sep])[1]=v1magic then begin
        YMAVmsg:=ClearMAVmessage;
        YMAVmsg:=NewYMavMessage(inlist, lineindex);
        if YMAVmsg.valid then begin
          if DoFilterYMAV(YMAVmsg) then begin
            inc(msgCounter);                           {Next message}
            WriteMsgToRaw(YMAVmsg, msgCounter);
            WriteMsgToData(YMAVmsg);
          end;
        end;
      end else
        inc(lineindex);
    until lineindex>=inlist.Count;
    gridData.AutoSizeColumns;
    gridRaw.AutoSizeColumn(0);
    StatusBar1.Panels[0].Text:='  '+IntToStr(msgCounter);
    FilterInfo;
    PageControl1.ActivePage:=tsRaw;
  finally
    gridRaw.EndUpdate;
    gridData.EndUpdate;
    inlist.Free;
    Screen.Cursor:=crDefault;
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
  if rgMsgFormat.ItemIndex=1 then
    result:=result+'_Y';
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
  Caption:=AppName;
  DefaultFormatSettings.DecimalSeparator:='.';
  StatusBar1.Panels[1].Text:=rgMsgFormat.Items[rgMsgFormat.ItemIndex];
  StatusBar1.Panels[2].Text:=rgOutputFormat.Items[rgOutputFormat.ItemIndex];
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
  colstyle: TTextStyle;

begin
  for i:=0 to 7 do
    gridSearchBytes.Cells[i, 0]:=IntToStr(i);
  colstyle:= gridSearchBytes.DefaultTextStyle;
  colstyle.Alignment:=taCenter;
  gridSearchBytes.DefaultTextStyle:=colstyle;

  acSaveRaw.Hint:=hntSaveRaw;
  acSaveData.Hint:=hntSaveData;
  acConvert.Hint:=hntConvert;
  acCreateTimestamp.Hint:=hntCreateTimestamp;
  acMerge.Hint:=hntMerge;
  acResetAll.Hint:=hntResetAll;
  acClearSearch.Hint:=hntClearSearch;
  acDecodeMAV.Hint:=hntDecodeMAV;
  acDecodeSR24.Hint:=hntDecodeSR24;
  acDecodeUART.Hint:=hntDecodeUART;
  acClose.Hint:=hntClose;
end;

procedure TForm1.mnGoFilterClick(Sender: TObject);
begin
  PageControl1.ActivePage:=tsSettings;
end;

procedure TForm1.rgMsgFormatClick(Sender: TObject);
begin
  StatusBar1.Panels[1].Text:=rgMsgFormat.Items[rgMsgFormat.ItemIndex];
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

procedure TForm1.acByteConvertExecute(Sender: TObject);
begin
  PageControl1.ActivePage:=tsSearch;

  {ToDo}
end;

procedure TForm1.acClearSearchExecute(Sender: TObject);
var
  i: integer;

begin
  for i:=0 to 7 do
    gridSearchBytes.Cells[i, 1]:='';
end;

procedure TForm1.acConvertExecute(Sender: TObject);
begin
  OpenDialog.Title:=titConvert;
  OpenDialog.Options:=OpenDialog.Options - [ofAllowMultiSelect];
  if OpenDialog.Execute then begin
    ConvertToBinary(OpenDialog.FileName);
  end;
end;

procedure TForm1.acDecodeMAVExecute(Sender: TObject);
begin
  OpenDialog.Title:=titDecode;
  OpenDialog.Options:=OpenDialog.Options - [ofAllowMultiSelect];
  if OpenDialog.Execute then begin
    CheckIfFilterIsSetMAV;
    DecodeRecordedCSVfromCGO3(OpenDialog.FileName);
  end;
end;

procedure TForm1.acDecodeSR24Execute(Sender: TObject);
begin
  OpenDialog.Title:=titDecode;
  OpenDialog.Options:=OpenDialog.Options - [ofAllowMultiSelect];
  if OpenDialog.Execute then begin
    CheckIfFilterIsSetSR24;
    DecodeRecordedCSVfromSR24(OpenDialog.FileName);
  end;
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
end;

procedure TForm1.acCreateTimestampExecute(Sender: TObject);

  procedure IntToSearchBytes(value: uint64; numbytes: byte);  {Numbytes ist 4 oder 8 (uint64}
  var
    i, shifted_value: uint64;

  begin
    shifted_value:=value;
    for i:=0 to numbytes-1 do begin
      gridSearchBytes.Cells[i, 1]:=IntToHex(shifted_value and 255, 2);
      shifted_value:=shifted_value shr 8;
    end;
  end;

  procedure UNIXtoUInt64;
  begin
    IntToSearchBytes(DTtoSek(now)*1000000, 8);                {gerade jetzt in µs}
  end;

  procedure UNIXtoUInt32;
  begin
    IntToSearchBytes(DTtoSek(now)*1000, 4);                   {gerade jetzt in ms}
  end;

  procedure UNIXtoUIntinSec;
  begin
    IntToSearchBytes(DTtoSek(now), 4);                        {gerade jetzt in s}
  end;

  procedure BootTimeToUInt32(number_ms: uint32);
  begin
    IntToSearchBytes(number_ms, 4);
  end;

  procedure IntStringToToSearchBytes;
  var
    stellen: byte;
    int_value: uint64;

  begin
    int_value:=StrToIntDef(edIntValue.Text, 2048);
    stellen:=2;
    if int_value>$FFFF then
      stellen:=4;
    if int_value>$FFFFFFFF then
      stellen:=8;
    edIntValue.Text:=IntToStr(int_value);
    IntToSearchBytes(int_value, stellen);
  end;

begin
  acClearSearch.Execute;
  case rgTimestamp.ItemIndex of
    0: UNIXtoUInt64;
    1: UNIXtoUInt32;
    2: UNIXtoUIntinSec;
    3: BootTimeToUInt32(1000);
    4: BootTimeToUInt32(60000);
    5: IntStringToToSearchBytes;
  end;
end;

procedure TForm1.acDecodeUARTExecute(Sender: TObject);
var
  RawFileFormat: byte;
  fn: string;

  procedure InvalidFile;
  begin
    StatusBar1.Panels[2].Text:=rsUndef;
    StatusBar1.Panels[4].Text:=fn+errInvalidFile;
  end;

  procedure SaleaeRecording;
  begin
    StatusBar1.Panels[2].Text:='to bin';
    StatusBar1.Panels[4].Text:=fn+' is another Saleae recording';
    ConvertToBinary(fn);
  end;

begin
  OpenDialog.Title:=titDecode;
  OpenDialog.Options:=OpenDialog.Options - [ofAllowMultiSelect];
  if OpenDialog.Execute then begin
    StatusBar1.Panels[0].Text:='';
    StatusBar1.Panels[1].Text:='';
    StatusBar1.Panels[3].Text:='';
    lbMsgType.Items.Clear;
    lbActionType.Items.Clear;
    lbMsgID.Items.Clear;
    lbSensorType.Items.Clear;
    fn:=OpenDialog.FileName;
    RawFileFormat:=CheckRawFileFormat(fn);
    case RawFileFormat of
      0: InvalidFile;
      1: SaleaeRecording;
      2: DecodeRecordedCSVfromCGO3(fn);
      3: DecodeRecordedCSVfromSR24(fn);
      4: DecodeSensorFile(fn);
    end;
  end;
end;

procedure TForm1.acHexViewExecute(Sender: TObject);
begin
  OpenDialog.Title:=titHexView;
  OpenDialog.Options:=OpenDialog.Options - [ofAllowMultiSelect];
  if OpenDialog.Execute then begin
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
    SaveDialog.FileName:=ChangeFileExt(OpenDialog.FileName, '')+'_Messages'+FilenameProposal;
  if SaveDialog.Execute then begin
    gridRaw.SaveToCSVFile(SaveDialog.FileName, dtsep);
    StatusBar1.Panels[4].Text:=rsSaved+SaveDialog.FileName;
  end;
end;

procedure TForm1.acMergeExecute(Sender: TObject);
var
  inlist, addlist_raw, addlist_data: TStringList;
  msgIDlist, msgtypelist, sensortypelist, actiontypelist: TStringList;
  fileindex: integer;
  maxColumns: integer;

{Format Raw-Ausgabe der Mergelist:
 Magic bleib Hex; Fix pard wird dezimal, der Rest entsprechend der Einstellungen}

  function YMAVmessageToMergelist(msg: TMAVmessage): string;
  var
    i: integer;

  begin
    result:=FormatDateTime(msgtpformat, msg.timepoint, [fdoInterval])+
            dtsep+IntToHex(msg.msgbytes[0], 2);
    for i:=1 to 7 do
      result:=result+dtsep+IntToStr(msg.msgbytes[i]);
    for i:=8 to msg.msglength+7 do
      result:=result+dtsep+OutputFormatByte(msg.msgbytes[i]);
  end;

  function SR24messageToMergelist(msg: TPayLoad; timept: string): string;
  var
    i, lenfix: integer;

  begin
    lenfix:=8;               {default lenght of fix part which is in decimal}
    case msg[3] of           {all messages with different length of fix part}
       2: lenfix:=6;
       4: lenfix:=6;
      20: lenfix:=5;
    end;
    result:=timept+dtsep+IntToHex(msg[0], 2)+dtsep+IntToHex(msg[1], 2);
    for i:=2 to lenfix-1 do
      result:=result+dtsep+IntToStr(msg[i]);
    for i:=lenfix to msg[2]+1 do
      result:=result+dtsep+OutputFormatByte(msg[i]);
  end;

  function YMAVdataToMergelist(msg: TMAVmessage): string;
  var
    i: integer;

  begin
    result:=FormatDateTime(msgtpformat, msg.timepoint, [fdoInterval])+  {Time}
            dtsep+IntToStr(msg.msgbytes[2])+                            {Sequ_No}
            dtsep+MsgIDToStr(msg.msgid)+                                {Msg_ID}
            dtsep+dtsep+SysIDToStr(msg.sysid)+                          {ActionType leer; Sys_ID}
            dtsep+TargetIDToStr(msg.targetid)+dtsep;                    {Target_ID, RSSI leer}
    if msg.msgid=255 then
      for i:=11 to msg.msglength+5 do  {Only the 'inner' payload of the BC-messages w/o its CRC}
        result:=result+dtsep+OutputFormatByte(msg.msgbytes[i])
    else                               {All other messages}
      for i:=8 to msg.msglength+7 do
        result:=result+dtsep+OutputFormatByte(msg.msgbytes[i]);
  end;

  function SR24dataToMergelist(msg: TPayLoad; timept: string): string;
  var
    i, lenfix, sequ_no: integer;

    function ChannelData: string;
    begin
      result:=dtsep+IntToStr(sequ_no)+dtsep+MessageTypeToStr(msg[3])+
              dtsep+dtsep+SysIDToStr(4)+dtsep+TargetIDToStr(1)+
              dtsep+IntToStr(GetRSSI(msg));
    end;

    function Telemetry: string;
    begin
      lenfix:=6;
      sequ_no:=sequ_no+msg[5]*256;
      result:=dtsep+IntToStr(sequ_no)+dtsep+MessageTypeToStr(msg[3])+
              dtsep+dtsep+SysIDToStr(1)+dtsep+TargetIDToStr(99)+dtsep;
    end;

    function Bind: string;
    begin
      lenfix:=0;
      result:=dtsep+IntToStr(sequ_no)+dtsep+MessageTypeToStr(msg[3])+
              dtsep+dtsep+dtsep+dtsep+rsBIND;
    end;

    function AddData: string;
    begin
      lenfix:=5;
      result:=dtsep+dtsep+MessageTypeToStr(msg[3])+
              dtsep+ActionTypeToStr(msg[4])+dtsep+dtsep+dtsep;
      case msg[4] of         {Payload abh von Action Type}
        5: begin lenfix:=0; result:=result+dtsep+'Sonar '+SwitchPos(msg[5]); end; {Sonar switch}
        9: begin lenfix:=0; result:=result+dtsep+'LED '+SwitchPos(msg[5]); end;   {Led switch}
      end;
    end;

  begin
    lenfix:=8;               {default lenght of fix part which is in decimal}
    result:=timept;
    sequ_no:=msg[4];

    case msg[3] of
       2: result:=result+Telemetry;
       4: result:=result+Bind;
      20: result:=result+AddData;
      else
        result:=result+ChannelData;
    end;

    if lenfix>0 then begin
      for i:=lenfix to msg[2]+1 do
        result:=result+dtsep+OutputFormatByte(msg[i]);
    end;
  end;

  procedure ReadOneYMAVfile(fn: string);
  var
    lineindex: integer;
    MAVmsg: TMAVmessage;

  begin
    inlist.LoadFromFile(fn);
    lineindex:=1;
    StatusBar1.Panels[0].Text:=IntToStr(inlist.Count-1);
    repeat
      if inlist[lineindex].Split([sep])[1]=v1magic then begin
        MAVmsg:=NewYMavMessage(inlist, lineindex);
        if MAVmsg.valid then begin
          msgIDlist.Add(Format('%.3d', [MAVmsg.msgid]));
          if MAVmsg.msgid=255 then
            sensortypelist.Add(Format('%.3d', [MAVmsg.msgbytes[13]]));
          if DoFilterYMAV(Mavmsg) then begin
            addlist_raw.Add(YMAVmessageToMergelist(MAVmsg));
            addlist_data.Add(YMAVdataToMergelist(MAVmsg));
          end;
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
    inlist.LoadFromFile(fn);
    lineindex:=1;
    StatusBar1.Panels[0].Text:=IntToStr(inlist.Count-1);
    repeat
      tp:=SaleaeTimeToOutputTime(inlist[lineindex].Split([sep])[0]);
      if SR24UARTreadMsg(SR24data, inlist, lineindex) then begin   {Message found and buffer filled}
        msgtypelist.Add(Format('%.3d', [SR24data[3]]));
        if SR24data[3]=20 then
          actiontypelist.Add(Format('%.3d', [SR24data[4]]));
        if DoFilterSR24(SR24data) then begin
          addlist_raw.Add(SR24messageToMergelist(SR24data, tp));
          addlist_data.Add(SR24dataToMergelist(SR24data, tp));
        end
      end else
        inc(lineindex);
    until lineindex>=inlist.Count;
    inlist.Clear;
  end;

  procedure FillgridRaw;
  var
    dataset: TStringList;
    header: TStrings;
    i: integer;

  begin
    dataset:=TStringList.Create;
    try
      dataset.StrictDelimiter:=true;
      dataset.Delimiter:=dtsep;
      gridRaw.RowCount:=1;                   {Löschen der vorherigen Anzeigen}
      gridRaw.RowCount:=addlist_raw.Count+1;
      maxcolumns:=MinimalBytesPerMessage;
      for i:=0 to addlist_raw.Count-1 do begin
        dataset.DelimitedText:=addlist_raw[i];
        if dataset.Count>maxcolumns then
          maxcolumns:=dataset.Count;
        gridRaw.Rows[i+1]:=dataset;
      end;
      gridRaw.ColCount:=maxcolumns;
      header:=gridRaw.Rows[0];
      header.StrictDelimiter:=true;
      header.Delimiter:=dtsep;
      addlist_raw.Insert(0, header.DelimitedText);
    finally
      dataset.Free;
      header.Free;
    end;
  end;

  procedure FillgridData;
  var
    dataset: TStringList;
    header: TStrings;
    i: integer;

  begin
    dataset:=TStringList.Create;
    try
      dataset.StrictDelimiter:=true;
      dataset.Delimiter:=dtsep;
      gridData.RowCount:=addlist_data.Count+1;
      maxcolumns:=MinimalBytesPerMessage;
      for i:=0 to addlist_data.Count-1 do begin
        dataset.DelimitedText:=addlist_data[i];
        if dataset.Count>maxcolumns then
          maxcolumns:=dataset.Count;
        gridData.Rows[i+1]:=dataset;
      end;
      gridData.ColCount:=maxcolumns;
      gridData.AutoSizeColumns;
      header:=gridData.Rows[0];
      header.StrictDelimiter:=true;
      header.Delimiter:=dtsep;
      addlist_data.Insert(0, header.DelimitedText);
    finally
      header.Free;
      dataset.Free;
    end;
  end;

  procedure ReadOneFile(fn: string);
  var
    fileformat: byte;

    procedure InvalidFile;
    begin
      StatusBar1.Panels[2].Text:=rsUndef;
      StatusBar1.Panels[4].Text:=fn+errInvalidFile;
    end;

    procedure SaleaeRecording;
    begin
      StatusBar1.Panels[2].Text:='to bin';
      StatusBar1.Panels[4].Text:=fn+' is another Saleae recording';
      ConvertToBinary(fn);
    end;

  begin
    fileformat:=CheckRawFileFormat(fn);              {Find out what file format it is}
    case fileformat of
      0: InvalidFile;
      1: SaleaeRecording;
      2: ReadOneYMAVfile(fn);                        {for merge}
      3: ReadOneSR24File(fn);
  //    2: DecodeRecordedCSVfromCGO3(fn);            {for single file}
  //    3: DecodeRecordedCSVfromSR24(fn);
  //    4: DecodeSensorFile(fn);
    end;
  end;

begin
  OpenDialog.Title:=titMerge;
  OpenDialog.Options:=OpenDialog.Options + [ofAllowMultiSelect];
  if OpenDialog.Execute then begin
    inlist:=TStringList.Create;
    addlist_raw:=TStringList.Create;
    addlist_data:=TStringList.Create;

    msgIDlist:=TStringList.Create;
    msgtypelist:=TStringList.Create;
    actiontypelist:=TStringList.Create;
    sensortypelist:=TStringList.Create;

    msgIDlist.Sorted:=true;
    msgIDlist.Duplicates:=dupIgnore;
    msgtypelist.Sorted:=true;
    msgtypelist.Duplicates:=dupIgnore;
    actiontypelist.Sorted:=true;
    actiontypelist.Duplicates:=dupIgnore;
    sensortypelist.Sorted:=true;
    sensortypelist.Duplicates:=dupIgnore;

    Screen.Cursor:=crHourGlass;
    MaxColumns:=NumPayloadBytes+lenfix;
    FillRawMergeHeader(MaxColumns);
    FillDataMergeHeader(MaxColumns);
    Application.ProcessMessages;
    MaxColumns:=MinimalBytesPerMessage;

    CheckIfFilterIsSetSR24;
    CheckIfFilterIsSetMAV;

    gridRaw.BeginUpdate;
    gridData.BeginUpdate;
    try
      if OpenDialog.Files.Count>1 then begin
        StatusBar1.Panels[0].Text:=IntToStr(OpenDialog.Files.Count);
        StatusBar1.Panels[4].Text:=OpenDialog.Files[0];
        for fileindex:=0 to OpenDialog.Files.Count-1 do
          ReadOneFile(OpenDialog.Files[fileindex]);

        addlist_raw.Sort;
        Application.ProcessMessages;
        addlist_data.Sort;
        Application.ProcessMessages;
        StatusBar1.Panels[0].Text:=IntToStr(addlist_raw.Count); {Anzahl der Messages aus beiden Dateien}
        FillgridRaw;
        FillgridData;

        FillMsgList(msgIDlist, lbMsgID, 0);
        FillMsgList(msgtypelist, lbMsgType, 1);
        FillMsgList(sensortypelist, lbSensorType, 2);
        FillMsgList(actiontypelist, lbActionType, 3);

        if cbSaveMerged.Checked then begin
          SaveDialog.FileName:=ChangeFileExt(OpenDialog.Files[0], '')+'_merged_raw'+csvext;
          addlist_raw.SaveToFile(SaveDialog.FileName);
          SaveDialog.FileName:=ChangeFileExt(OpenDialog.Files[0], '')+'_merged_data'+csvext;
          addlist_data.SaveToFile(SaveDialog.FileName);
        end;
        gridData.AutoSizeColumns;
        gridRaw.AutoSizeColumn(0);
        PageControl1.ActivePage:=tsRaw;
      end else
        StatusBar1.Panels[4].Text:=hntMoreFiles;
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
      Screen.Cursor:=crDefault;
    end;
  end;
end;


end.

