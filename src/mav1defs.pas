unit mav1defs;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, math;

const
  dzfl='0.0';                {Formatierung für FormatFloat}
  ctfl='0.00';
  mlfl='0.000';
  coordfl8='0.00000000';

  v1magic='0xFE';
  BCmagic=$BC;
  NumPayloadBytes=255;       {reserved lenght for data}
  lenfix=10;                 {+ fix part}

  rsUnknown_='Unknown_';
  rsUndef_='Undef_';


type
  TMAVmessage = record
    time: TDateTime;
    msglength: integer;
    msgbytes: array[0..NumPayLoadBytes+lenfix] of byte;
    sysid: byte;
    targetid: byte;
    msgid: byte;
    valid: boolean;
  end;

{Public functions and procedures}

function MavGetIntFromBuf(msg: TMAVmessage; pos, numbytes: integer): uint64; {Position/Anzahl Bytes}
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


implementation

function MavGetIntFromBuf(msg: TMAVmessage; pos, numbytes: integer): uint64; {Position/Anzahl Bytes}
var
  i: integer;

begin
  result:=0;
  for i:=0 to numbytes-1 do begin
    result:=result+msg.msgbytes[i+pos]*(256**i);
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
  for i:=0 to lenfix do
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
    3, 200: result:='Gimbal';                 {200 in nested Sensor data}
    4: result:='Remote';
    6: result:='SysID6';
    10: result:='YQGC';
    88: result:='Undef';
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
    99: result:='Remote';                    {Platzhalter für virtuelle Sender ID}
  end;
end;

function SensorTypeToStr(id: byte): string;  {Message BC}
begin
  result:=rsUnknown_+' '+IntToStr(id);
  case id of
    0:   result:='Heartbeat?';
    1:   result:='SYS_STATUS';
    2:   result:='System_Time';
    24:  result:='GPS_RAW';
    27:  result:='RAW_IMU';
    29:  result:='SCALED_PRESSURE';
    30:  result:='ATTITUDE';
    33:  result:='GLOBAL_POSITION';
    35:  result:='RC_CHANNELS_RAW';
    36:  result:='SERVO_OUTPUT_RAW';
    42:  result:='MISSION_CURRENT';
    51:  result:='MISSION_REQUEST_INT';
    52:  result:='Sys_type?';                      {Text: CGO3_Plus / TyphoonH}
    62:  result:='NAV_CONTROLLER_OUTPUT';
    65:  result:='RC_CHANNELS';
    74:  result:='VRF_HUD';
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
    36: result:='Data_request?';
  end;
end;

function SeverityToStr(severity: byte): string;
begin
  result:=IntToStr(severity);
  case severity of
    0: result:='EMERGENCY'; {System is unusable. This is a "panic" condition}
    1: result:='ALERT';     {Action should be taken immediately. Indicates error
                             in non-critical systems}
    2: result:='CRITICAL';  {Action must be taken immediately. Indicates failure
                             in a primary system}
    3: result:='ERROR';     {Indicates an error in secondary/redundant systems}
    4: result:='WARNING';   {Indicates about a possible future error if this
                             is not resolved within a given timeframe. Example
                             would be a low battery warning}
    5: result:='NOTICE';    {An unusual event has occurred, though not an error
                             condition. This should be investigated for the
                             root cause.}
    6: result:='INFO';      {Normal operational messages. Useful for logging.
                             No action is required for these messages.}
    7: result:='DEBUG';     {Useful non-operational messages that can assist in
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
  la:=MavGetIntFromBuf(data, pos, 4);
  lo:=MavGetIntFromBuf(data, pos+4, 4);
  lat:=la/10000000;
  lon:=lo/10000000;
  alt:=MavGetFloatFromBuf(data, pos+8);
  if (la<>0) or (lo<>0) then
    result:=true;
end;

procedure MavGetInt123(data: TMAVmessage; pos: integer; var v1, v2, v3: int16);
begin
  v1:=MavGetIntFromBuf(data, pos, 2);
  v2:=MavGetIntFromBuf(data, pos+2, 2);
  v3:=MavGetIntFromBuf(data, pos+4, 2);
end;

procedure MavGetuInt123(data: TMAVmessage; pos: integer; var v1, v2, v3: uint16);
begin
  v1:=MavGetIntFromBuf(data, pos, 2);
  v2:=MavGetIntFromBuf(data, pos+2, 2);
  v3:=MavGetIntFromBuf(data, pos+4, 2);
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

end.

