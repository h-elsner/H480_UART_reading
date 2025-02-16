unit mav1defs;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, math, DateUtils;

const
  dzfl='0.0';                {Formatierung für FormatFloat}
  ctfl='0.00';
  mlfl='0.000';
  coordfl8='0.00000000';

  NumPayloadBytes=255;       {reserved lenght for data}

  LengthFixPartBC=6;
  LengthFixPartFD=10;
  LengthFixPartFE=8;
  MagicBC=$BC;
  MagicFD=$FD;
  MagicFE=$FE;
  MagicFE_AsText='0xFE';

  rsUnknown_='Unknown_';
  rsUndef_='Undef_';


type
  TMAVmessage = record
    time: TDateTime;
    msglength: integer;
    msgbytes: array[0..NumPayLoadBytes+LengthFixPartFE+2] of byte;
    sysid: byte;
    targetid: byte;
    msgid: byte;
    msgid32: uint32;
    valid: boolean;
  end;

{Public functions and procedures}
function CRC16X25(const msg: TMAVmessage; LengthFixPart: byte): uint16;
function CheckCRC16X25(const msg: TMAVmessage; LengthFixPart: byte): boolean;
procedure CRC_accumulate(const b: byte; var crcAccum: uint16);
function CRC16MAV(const msg: TMAVmessage; LengthFixPart: byte; startpos: byte=1): uint16;
function CheckCRC16MAV(const msg: TMAVmessage; LengthFixPart: byte): boolean;

function MavGetUInt64(msg: TMAVmessage; pos: integer): uint64; {Position/Anzahl Bytes}
function MavGetFloatFromBuf(msg: TMAVmessage; pos: integer): single; {Position, Länge immer 4}
function MavGetUInt16(msg: TMAVmessage; pos: integer): uint16;
function MavGetInt16(msg: TMAVmessage; pos: integer): int16;
function MavGetUInt32(msg: TMAVmessage; pos: integer): uint32;
function MavGetInt32(msg: TMAVmessage; pos: integer): int32;

function ClearMAVmessage: TMAVmessage;
function MsgIDToStr(id: byte): string;
function SysIDToStr(id: byte): string;
function TargetIDToStr(id: byte): string;
function SensorTypeToStr(id: byte): string;  {Message BC}
function MAV_PARAM_TYPEtoStr(const id: byte): string; {Specifies the datatype of a MAVLink parameter}
function MAV_RESULTtoStr(const id: byte): string;
function YGC_TypeToStr(y: byte): string;
function YGC_CommandToStr(y: byte): string;
function SeverityToStr(severity: byte): string;

function IntToYaw(v: integer): string;
function IntToAngle(v: integer): string;
function IntToPan(v: integer): string;
function IntToTilt(v: integer): string;
function IntToPanMode(v: integer): string;
function IntToTiltMode(v: integer): string;
function IntToLatLon(v: int32): string;
function FloatToAlt(v: single): string;
function IntToAlt(v: int32): string;
function IntToAccuracy(v: integer): string;
function IntToRCAngle(v: int16): string;
function IntToSpeed(v: integer): string;
function GPSswitchToStr(g: byte): string;
function GimbalAngleToStr(a: int16): string;
function MavGetGPSdata(data: TMAVmessage; pos: integer;
                    var lat, lon, alt: single): boolean;  {Get lat, lon and alt from GPS data}
function VoltToStr(v: byte): string;
function MavGetChValue(data: TMAVmessage; chnr: byte; pos: integer=27): uint16; {Channel no from 1..12 or 1..24}
function GPSaccHToStr(const g: byte): string;
procedure MavGetInt123(data: TMAVmessage; pos: integer; var v1, v2, v3: int16);
procedure MavGetuInt123(data: TMAVmessage; pos: integer; var v1, v2, v3: uint16);
function GetSerialNumber(const msg: TMAVmessage; pos: byte): string;
function GetSystemTime(const data: TMAVmessage; pos: integer; var time: TDateTime): string;

implementation

{Tabelle CCITT X25 CRC aus ST16 MavLinkPackage.java
 b ... Array of Byte
 ln... Länge Payload (Byte 1) der Message, Byte 0=$BC wird nicht genutzt
       Schleife über Rest der Message 0...Länge Payload+Länge Fixpart-3}

function CRC16X25(const msg: TMAVmessage; LengthFixPart: byte): uint16;
const
  Crc16Tab: array[0..255] of Word = (
    $0000, $1189, $2312, $329B, $4624, $57AD, $6536, $74BF,
    $8C48, $9DC1, $AF5A, $BED3, $CA6C, $DBE5, $E97E, $F8F7,
    $1081, $0108, $3393, $221A, $56A5, $472C, $75B7, $643E,
    $9CC9, $8D40, $BFDB, $AE52, $DAED, $CB64, $F9FF, $E876,
    $2102, $308B, $0210, $1399, $6726, $76AF, $4434, $55BD,
    $AD4A, $BCC3, $8E58, $9FD1, $EB6E, $FAE7, $C87C, $D9F5,
    $3183, $200A, $1291, $0318, $77A7, $662E, $54B5, $453C,
    $BDCB, $AC42, $9ED9, $8F50, $FBEF, $EA66, $D8FD, $C974,
    $4204, $538D, $6116, $709F, $0420, $15A9, $2732, $36BB,
    $CE4C, $DFC5, $ED5E, $FCD7, $8868, $99E1, $AB7A, $BAF3,
    $5285, $430C, $7197, $601E, $14A1, $0528, $37B3, $263A,
    $DECD, $CF44, $FDDF, $EC56, $98E9, $8960, $BBFB, $AA72,
    $6306, $728F, $4014, $519D, $2522, $34AB, $0630, $17B9,
    $EF4E, $FEC7, $CC5C, $DDD5, $A96A, $B8E3, $8A78, $9BF1,
    $7387, $620E, $5095, $411C, $35A3, $242A, $16B1, $0738,
    $FFCF, $EE46, $DCDD, $CD54, $B9EB, $A862, $9AF9, $8B70,
    $8408, $9581, $A71A, $B693, $C22C, $D3A5, $E13E, $F0B7,
    $0840, $19C9, $2B52, $3ADB, $4E64, $5FED, $6D76, $7CFF,
    $9489, $8500, $B79B, $A612, $D2AD, $C324, $F1BF, $E036,
    $18C1, $0948, $3BD3, $2A5A, $5EE5, $4F6C, $7DF7, $6C7E,
    $A50A, $B483, $8618, $9791, $E32E, $F2A7, $C03C, $D1B5,
    $2942, $38CB, $0A50, $1BD9, $6F66, $7EEF, $4C74, $5DFD,
    $B58B, $A402, $9699, $8710, $F3AF, $E226, $D0BD, $C134,
    $39C3, $284A, $1AD1, $0B58, $7FE7, $6E6E, $5CF5, $4D7C,
    $C60C, $D785, $E51E, $F497, $8028, $91A1, $A33A, $B2B3,
    $4A44, $5BCD, $6956, $78DF, $0C60, $1DE9, $2F72, $3EFB,
    $D68D, $C704, $F59F, $E416, $90A9, $8120, $B3BB, $A232,
    $5AC5, $4B4C, $79D7, $685E, $1CE1, $0D68, $3FF3, $2E7A,
    $E70E, $F687, $C41C, $D595, $A12A, $B0A3, $8238, $93B1,
    $6B46, $7ACF, $4854, $59DD, $2D62, $3CEB, $0E70, $1FF9,
    $F78F, $E606, $D49D, $C514, $B1AB, $A022, $92B9, $8330,
    $7BC7, $6A4E, $58D5, $495C, $3DE3, $2C6A, $1EF1, $0F78);

var i: integer;

begin
  result:=$FFFF;                             {CRC Initializing}
  for i:=2 to LengthFixPart+msg.msglength-1 do begin
    result:=((result shr 8) and $00FF) xor CRC16Tab[(msg.msgbytes[i] xor result) and $00FF];
  end;
end;

function CheckCRC16X25(const msg: TMAVmessage; LengthFixPart: byte): boolean;
begin
  result:=(CRC16X25(msg, LengthFixPart+2)=0);
end;

procedure CRC_accumulate(const b: byte; var crcAccum: uint16);
var
  tmp: uint8;

begin
  tmp:=b xor (crcAccum and $00FF);
  tmp:=tmp xor (tmp shl 4);
  crcAccum:=(crcAccum shr 8) xor (tmp shl 8) xor (tmp shl 3) xor (tmp shr 4);
end;

{from checksum.h of MavlinkLib-master}
function CRC16MAV(const msg: TMAVmessage; LengthFixPart: byte; startpos: byte=1): uint16;
var
  i: integer;

begin
  result:=$FFFF;
  for i:=startpos to LengthFixPart+msg.msglength-1 do begin
    CRC_accumulate(msg.msgbytes[i], result);
  end;
  CRC_accumulate(0, result);
end;

function CheckCRC16MAV(const msg: TMAVmessage; LengthFixPart: byte): boolean;
begin
  result:=(CRC16MAV(msg, LengthFixPart)=MavGetUInt16(msg, LengthFixPart+msg.msglength));
end;

function MavGetUInt64(msg: TMAVmessage; pos: integer): uint64; {Position/Anzahl Bytes}
var
  i: integer;

begin
  result:=0;
  for i:=0 to 7 do begin
    result:=result+msg.msgbytes[pos+i]*(256**i);
  end;
end;

function MavGetFloatFromBuf(msg: TMAVmessage; pos: integer): single; {Position, Länge immer 4}
var i: integer;
    wfl: packed array[0..3] of Byte;
    wx: Single absolute wfl;

begin
  result:=0;
  for i:=0 to 3 do                               {Endianess prüfen (to/downto)}
    wfl[i]:=msg.msgbytes[i+pos];                 {4 byte aus Buffer ausschneiden}
  result:=wx;                                    {Typecast mittels absolute}
end;

function MavGetInt16(msg: TMAVmessage; pos: integer): int16;
begin
  result:=msg.msgbytes[pos]+msg.msgbytes[pos+1]*256;
end;

function MavGetUInt16(msg: TMAVmessage; pos: integer): uint16;
begin
  result:=msg.msgbytes[pos]+msg.msgbytes[pos+1]*256;
end;

function MavGetUInt32(msg: TMAVmessage; pos: integer): uint32;
var
  i: integer;

begin
  result:=0;
  for i:=0 to 3 do begin
    result:=result+msg.msgbytes[i+pos]*(256**i);
  end;
end;

function MavGetInt32(msg: TMAVmessage; pos: integer): int32;
var
  i: integer;

begin
  result:=0;
  for i:=0 to 3 do begin
    result:=result+msg.msgbytes[i+pos]*(256**i);
  end;
end;

function ClearMAVmessage: TMAVmessage;
var
  i: integer;

begin
  result.msglength:=0;
  result.sysid:=0;
  result.targetid:=0;
  result.valid:=false;
  for i:=0 to LengthFixPartFE do
    result.msgbytes[i]:=0;                    {Fix part empty}
end;

function MsgIDToStr(id: byte): string;
begin
  result:=rsUnknown_+' '+IntToStr(id)+' (0x'+HexStr(id, 2)+')';
  case id of
    0: result:='HEARTBEAT';                   {1Hz}
    1: result:='Attitude';                    {Frequently}
    2: result:='Telemetry_5GHz';
    3: result:='Gimbal position';             {Sys_Id Camera?; 1Hz }
    8: result:='C-GPS_5GHz';
    20: result:='PARAM_REQUEST_READ';
    76: result:='Follow_on_param_request';    {Command long?? Passt nicht}
    255: result:='SensorData';
  end;
end;

function SysIDToStr(id: byte): string;        {Sent from}
begin
  result:=rsUnknown_+' '+IntToStr(id)+' (0x'+HexStr(id, 2)+')';
  case id of
    1: result:='Autopilot';                   {Flight controller (Autopilot)}
    2: result:='Gimbal';
    3: result:='Gimbal';
    4: result:='Remote';
    6: result:='SysID6';
    10: result:='YQGC';
    88: result:='Undef';
    200: result:='Tool/GUI';
  end;
end;

function TargetIDToStr(id: byte): string;     {Send to}
begin
  result:=rsUnknown_+' '+IntToStr(id)+' (0x'+HexStr(id, 2)+')';
  case id of
    0: result:='All (broadcast)';
    1: result:='Autopilot';                   {Flight controller (Autopilot)}
    2: result:='Gimbal';
    3: result:='Camera';
    5: result:='WiFi? (5)';
    10: result:='YQGC';
    88: result:='Undef';
    200: result:='Tool/GUI';
    99: result:='Remote';                    {Platzhalter für virtuelle Sender ID}
  end;
end;

{https://github.com/mavlink/mavlink/tree/master/message_definitions/v1.0
 https://github.com/mavlink/mavlink/blob/master/message_definitions/v1.0/common.xml
 }
function SensorTypeToStr(id: byte): string;  {Message BC}
begin
  result:=rsUnknown_+IntToStr(id);
  case id of
    0:   result:='Heartbeat';
    1:   result:='SYS_STATUS';
    2:   result:='SYSTEM_TIME';
    21:  result:='PARAM_REQUEST_LIST';       {Request all parameters of this component.
                                              After this request, all parameters are emitted.
                                              The parameter microservice is documented at
                                              https://mavlink.io/en/services/parameter.html}
    22:  result:='PARAM_VALUE';
    23:  result:='PARAM_SET';
    24:  result:='GPS_RAW_INT';
    25:  result:='GPS_STATUS';
    27:  result:='RAW_IMU';
    29:  result:='SCALED_PRESSURE';
    30:  result:='ATTITUDE';
    32:  result:='LOCAL_POSITION_NED';
    33:  result:='GLOBAL_POSITION';
    35:  result:='RC_CHANNELS_RAW';
    36:  result:='SERVO_OUTPUT_RAW';
    42:  result:='MISSION_CURRENT';
    51:  result:='MISSION_REQUEST_INT';
    52:  result:='System_type';                    {Text: CGO3_Plus / TyphoonH}
    56:  result:='SERIAL_NUMBER';
    62:  result:='NAV_CONTROLLER_OUTPUT';
    65:  result:='RC_CHANNELS';
    74:  result:='VRF_HUD';
    76:  result:='COMMAND_LONG';
    77:  result:='COMMAND_ACK';
    150: result:='SENSOR_OFFSETS';
    163: result:='AHRS';                           {Attitude and Heading Reference System}
    165: result:='HW_STATUS';
    172: result:='DATA96';
    173: result:='RANGEFINDER';
    178: result:='AHRS2';
    193: result:='EKF_STATUS_REPORT';              {Extended Kalman Filter}
    253: result:='STATUS_TEXT';
  end;
end;

function MAV_PARAM_TYPEtostr(const id: byte): string;        {Specifies the datatype of a MAVLink parameter}
begin
  result:='PARAM_TYPE '+intToStr(id);
  case id of
    1: result:='UINT8';
    2: result:='INT8';
    3: result:='UINT16';
    4: result:='INT16';
    5: result:='UINT32';
    6: result:='INT32';
    7: result:='UINT64';
    8: result:='INT64';
    9: result:='REAL32';
    10: result:='REAL64';
  end;
end;

function MAV_RESULTtoStr(const id: byte): string;
begin
  result:='RESULT '+intToStr(id);
  case id of
    0: result:='ACCEPTED';
    1: result:='TEMPORA';
    2: result:='DENIED';
    3: result:='UNSUPPORTED';
    4: result:='FAILED';
    5: result:='IN_PROGRESS';
    6: result:='CANCELLED';
    7: result:='COMMAND_LONG_ONLY';
    8: result:='COMMAND_INT_ONLY';
    9: result:='COMMAND_UNSUPPORTED_MAV_FRAME';
  end;
end;

function YGC_TypeToStr(y: byte): string;
begin
  result:=rsUndef_+IntToStr(y);
  case y of
    1: result:='GYRO_POWER';
    2: result:='EULER_ANGLE';
    3: result:='ACC';
    5: result:='TEMP_DIFF';
    6: result:='MSTATUS';
    18: result:='Undef 0x12 (18) 1Hz';
    254: result:='FW_Info';
  end;
end;

function YGC_CommandToStr(y: byte): string;
begin
  result:='YGC_Request ('+intToStr(y)+')';
  case y of
    20: result:='Front_cali?';
    24: result:='Read_SWversion';
    36: result:='Heartbeat';                    {Data request; starts sending data messages from Gimbal}
  end;
end;

function SeverityToStr(severity: byte): string;
begin
  result:=IntToStr(severity);
  case severity of
    0: result:='EMERGENCY';  {System is unusable. This is a "panic" condition}
    1: result:='ALERT    ';  {Action should be taken immediately. Indicates error
                             in non-critical systems}
    2: result:='CRITICAL ';  {Action must be taken immediately. Indicates failure
                             in a primary system}
    3: result:='ERROR    ';  {Indicates an error in secondary/redundant systems}
    4: result:='WARNING  ';  {Indicates about a possible future error if this
                             is not resolved within a given timeframe. Example
                             would be a low battery warning}
    5: result:='NOTICE   ';  {An unusual event has occurred, though not an error
                             condition. This should be investigated for the
                             root cause.}
    6: result:='INFO     ';  {Normal operational messages. Useful for logging.
                             No action is required for these messages.}
    7: result:='DEBUG    ';  {Useful non-operational messages that can assist in
                             debugging. These should not occur during normal
                             operation}
  end;
end;

function IntToYaw(v: integer): string;
begin
  result:=FormatFloat(dzfl, 180/16000*v-180);
end;

function IntToAngle(v: integer): string;
begin
  result:=FormatFloat(dzfl, -90/7650*v);
end;

function IntToPan(v: integer): string;
begin
//  result:=FormatFloat(dzfl, 100/2048*(v-2048));
  result:=intToStr(v);
end;

function IntToTilt(v: integer): string;
begin
  result:=FormatFloat(dzfl, -(v-683)*90/2729);
end;

function IntToTiltMode(v: integer): string;
begin
  result:=rsUndef_+IntToStr(v);
  case v of
    2048: result:='Neutral';                    {sollte eigentlich nicht auftreten}
    2184: result:='Angle';                      {+10%}
    3412: result:='Velocity';                   {+100%}
  end;
end;

function IntToPanMode(v: integer): string;
begin
  result:=rsUndef_+IntToStr(v);
  case v of
    683: result:='Follow';                      {-100%}
    1433: result:='Team';                       {-45%}
    1502: result:='Follow controllable';        {-40%}
    2048: result:='Neutral';                    {sollte eigentlich nicht auftreten}
    3412: result:='Global';                     {+100%}
  end;
end;

function IntToProzent(v: integer): string;
begin
  result:='Slide '+IntToStr(v)+' ('+IntToHex(v and 255, 2)+' '+            {nwt}
                                    IntToHex((v shr 8) and 255, 2)+')';    {hwt}
  case v of
    0: result:=   '-150%';
    683: result:= '-100%';
    1433: result:='-45%';
    1502: result:='-40%';
    2048: result:='0%';
    2184: result:='+10%';
    3412: result:='+100%';
    4095: result:='+150%';
  end;
end;

function IntToLatLon(v: integer): string;
begin
  result:=FormatFloat(coordfl8, v*0.0000001);
end;

function FloatToAlt(v: single): string;
begin
  result:=FormatFloat(ctfl, v);
end;

function IntToAlt(v: integer): string;
begin
  result:=FormatFloat(dzfl, v*0.01);
end;

function IntToAccuracy(v: integer): string;
begin
  result:=FormatFloat(dzfl, v*0.1);
end;

function IntToRCAngle(v: int16): string;
begin
  result:=FormatFloat(dzfl, v*0.01);
end;

function IntToSpeed(v: integer): string;
begin
  result:=FormatFloat(dzfl, v*0.1);
end;

function GPSswitchToStr(g: byte): string;
begin
  result:='0x'+IntToHex(g, 2);         {currently always 0x0B}
end;

function GimbalAngleToStr(a: int16): string;
begin
  result:=FormatFloat(ctfl, a*0.01);
end;

function MavGetGPSdata(data: TMAVmessage; pos: integer;
                     var lat, lon, alt: single): boolean;  {Get lat, lon and alt from GPS data}
var
  la, lo: int32;

begin
  result:=false;
  la:=MavGetInt32(data, pos);
  lo:=MavGetInt32(data, pos+4);
  lat:=la/10000000;
  lon:=lo/10000000;
  alt:=MavGetFloatFromBuf(data, pos+8);
  if (la<>0) or (lo<>0) then
    result:=true;
end;

procedure MavGetInt123(data: TMAVmessage; pos: integer; var v1, v2, v3: int16);
begin
  v1:=MavGetInt16(data, pos);
  v2:=MavGetInt16(data, pos+2);
  v3:=MavGetInt16(data, pos+4);
end;

procedure MavGetuInt123(data: TMAVmessage; pos: integer; var v1, v2, v3: uint16);
begin
  v1:=MavGetUInt16(data, pos);
  v2:=MavGetUInt16(data, pos+2);
  v3:=MavGetUInt16(data, pos+4);
end;

function  VoltToStr(v: byte): string;
begin
  result:=FormatFloat(dzfl, v*0.1+5)+'V';
end;

function MavGetChValue(data: TMAVmessage; chnr: byte; pos: integer=27): uint16; {Channel no from 1..12 or 1..24}
var
  n: byte;

begin
  n:=((chnr-1) div 2)*3+pos;
  if (chnr and 1)=0 then begin                           {even channel no Ch0...}
    result:=lo(data.msgbytes[n+1])*256+data.msgbytes[n+2];
  end else begin                                         {uneven channel no Ch1...}
    result:=data.msgbytes[n]*16+hi(data.msgbytes[n+1]);
  end;
end;

function  GPSaccHToStr(const g: byte): string;
begin
  result:=FormatFloat(dzfl, g/20);
end;

function GetSerialNumber(const msg: TMAVmessage; pos: byte): string;
begin
  result:='';
  result:=IntToHex(MavGetUInt32(msg, pos), 8)+'-'+
          IntToHex(MavGetUInt32(msg, pos+4), 8)+'-'+
          IntToHex(MavGetUInt32(msg, pos+8), 8);
end;

function GetSystemTime(const data: TMAVmessage; pos: integer; var time: TDateTime): string;
begin
  time:=UNIXtoDateTime(MavGetUInt64(data, pos) div 1000000);      {µs}
  result:=FormatDateTime('YYYY-MM-DD hh:nn:ss', time);
end;

end.

