'----------------------------------------------------------------------------------------
'                               Radio Alarm Clock - Firmware
'
'
'
' This program is open source. You can redistribute it and/or modify it under the terms
' of the GNU General Public License as published by the Free Software Foundation, either
' version 3 of the License, or (at your option) any later version.
'
' This program is distributed WITHOUT ANY WARRANTY; without even the implied warranty
' of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
' License for more details.
'
' <https://www.gnu.org/licenses/>
'
'
'
'                                                  June 2022 - coreWeaver / ioConnected
'----------------------------------------------------------------------------------------

$regfile = "m328pdef.dat"
$crystal = 16000000
$baud = 19200
$hwstack = 256
$swstack = 256
$framesize = 256

Config Single = Scientific , Digits = 3

'---- UART Config ----
Config Com1 = 19200 , Synchrone = 0 , Parity = None , Stopbits = 1 , Databits = 8 , Clockpol = 0
Open "com1:" For Binary As #1
Config Serialin = Buffered , Size = 50 , Bytematch = All
Disable Serial

'---- LCD Config ----
Config Lcdpin = Pin , Db4 = Portd.4 , Db5 = Portd.5 , Db6 = Portd.6 , Db7 = Portd.7 , E = Portd.3 , Rs = Portd.2
Config Lcd = 20 * 4
Initlcd
Cursor Off
Cls

'---- Timer 0 is used for LCD Backlight Time Interval ----
Config Timer0 = Timer , Prescale = 1024
Const Timer0_reload = 100                                   'OVF in 10mS (100Hz)
On Timer0 Timer0_isr
Enable Timer0
Stop Timer0

'---- Timer 2 is used for Alarm ----
Config Timer2 = Timer , Prescale = 1024
Const Timer2_reload = 100                                   'OVF in 10mS (100Hz)
On Timer2 Timer2_isr
Enable Timer2
Stop Timer2

'---- LCD Backlight ----
Config Portb.3 = Output
Lcd_bkl Alias Portb.3
Set Lcd_bkl

'---- Test LEDs (sourcing mode) ----
Config Portc.3 = Output
Led Alias Portc.3
Reset Led

'---- Buzzer ----
Speaker Alias Portc.2

'---- Keys ----
Config Pinb.0 = Input
Portb.0 = 1
Set_key Alias Pinb.0

Config Pinb.1 = Input
Portb.1 = 1
Plus_key Alias Pinb.1

Config Pinb.2 = Input
Portb.2 = 1
Minus_key Alias Pinb.2

Config Pinb.4 = Input
Portb.4 = 1
Snooze_key Alias Pinb.4

Config Pinb.5 = Input
Portb.5 = 1
Radio_key Alias Pinb.5

Config Pinc.1 = Input
Portc.1 = 1
Tune_key Alias Pinc.1

'---- Temperature Sensors (DS1820) / 1Wire Protocol ----
Config 1wire = Portc.0
$lib "ds1307clock.lib"                                      ' modified lib

'---- I2C's SCL and SDA pins ----
Config Sda = Portc.4
Config Scl = Portc.5

'---- Ds1307 RTC - I2C Address ----
Const Ds1307w = &HD0
Const Ds1307r = &HD1

'---- Radio Module - I2C Address ----
Const Tea5767w = &HC0
Const Tea5767r = &HC1

Const Module_frequency = 32768
Const Frequency = 225000

Config Clock = User                                         ' this will dim the bytes automatic
Config Date = Mdy , Separator = Slash

'---- Special LCD characters ----
Deflcdchar 0 , 12 , 14 , 15 , 15 , 15 , 15 , 15 , 15
Deflcdchar 1 , 15 , 15 , 15 , 15 , 15 , 15 , 14 , 12
Deflcdchar 2 , 31 , 15 , 7 , 32 , 32 , 32 , 32 , 32
Deflcdchar 3 , 31 , 30 , 28 , 32 , 32 , 32 , 32 , 32
Deflcdchar 4 , 32 , 32 , 32 , 32 , 32 , 7 , 15 , 31
Deflcdchar 5 , 32 , 32 , 32 , 32 , 32 , 28 , 30 , 31
Deflcdchar 6 , 6 , 14 , 30 , 30 , 30 , 30 , 30 , 30
Deflcdchar 7 , 30 , 30 , 30 , 30 , 30 , 30 , 14 , 6

'---- Variables ----
Dim Pll As Single
Dim Pll_freq As Word
Dim 1st As Byte
Dim 2nd As Byte
Dim 3rd As Byte
Dim 4th As Byte
Dim 5th As Byte

Dim Radio_freq(9) As Single                                 'Max 9 Radio Stations
Dim Station(9) As String * 16
Dim F(9) As String * 7

Dim R_station As Byte
Dim Radio_station As Byte
Dim W As String * 5

Dim Timeout As Integer
Dim Timeout_sec As Integer

Dim Temp_var As Integer

Dim Weekday As Byte
Dim X As Byte
Dim Pos1 As Byte , Pos2 As Byte , Pos3 As Byte , Pos4 As Byte

Dim Hh As Integer , Mm As Integer , Ss As Integer
Dim Dd As Integer , Mn As Integer , Yy As Integer
Dim Al_hh As Integer , Al_mm As Integer , Al_ss As Integer

Dim Hour As String * 2 , Minute As String * 2 , Second As String * 2
Dim Day As String * 2 , Month As String * 2 , Year As String * 2
Dim H1 As String * 1 , H2 As String * 1 , M1 As String * 1 , M2 As String * 1

Dim Newtime As String * 8 , Newdate As String * 8
Dim Alarm_time As String * 8

Dim Al_hh_str As String * 2 , Al_mm_str As String * 2

Dim Alarm_type As Integer

Dim Is_leap_year As Bit
Reset Is_leap_year

Dim Mycounter As Integer
Dim A As Integer

Dim Exitvar As Bit
Reset Exitvar

Dim Ch As Byte

Dim Light_on As Bit
Reset Light_on

Dim Long_press As Bit
Reset Long_press

Dim Press_time As Integer
Press_time = 0

Dim Alarm_is_set As Bit
Reset Alarm_is_set

Dim Set_key_time As Integer

Dim Timeout_flag As Bit
Reset Timeout_flag

Dim Snooze_long_press As Bit
Reset Snooze_long_press

Dim Snooze_press_time As Integer

Dim Do_once As Bit
Reset Do_once

Dim Radio As Bit
Reset Radio

Dim Exit_var As Bit
Reset Exit_var

Dim Rtc_menu As Integer

Dim Exit_menu As Bit
Reset Exit_menu

Dim Lcd_on As Bit
Reset Lcd_on

Dim Alarm_duration As Integer

Dim Al_type As Integer
Dim Once As Bit
Reset Once

Dim Timeout2 As Integer
Dim Timeout2_sec As Integer

Dim T2_1min_flag As Bit , T2_3min_flag As Bit
Dim Repeat_alarm As Integer
Dim Alarm_flag As Bit

Dim Timeout3 As Integer
Dim Onetime As Bit
Reset Onetime
Dim Dat_s As String * 8
Dim Decival As Byte
Dim Dsid(8) As Byte
Dim Nr As Byte
Dim Sc(9) As Byte
Dim T As Long
Dim T1 As Integer
Dim Tmp As Byte
Dim Temperature As String * 5
Dim Tempdecimal As String * 2
Dim Tempunit As String * 3

Dim U As Integer
Dim S_nr As Integer
Dim V As String * 1
Dim Ts_len As Integer

Dim Timeout_alarm_off As Integer

Dim Rxvalue As String * 32
Rxvalue = ""

Dim Rx_str As String * 32
Dim Rx_flag As Bit
Reset Rx_flag

Dim Rx_header As String * 1
Dim Rx_lastchar As String * 1
Dim S_pos As String * 1
Dim Station_pos As Byte
Dim At_pos As Integer , Name_chars As Integer , Freq_chars As Integer

Dim I As Integer

Dim Rd_1st_byte As Byte

Dim Address As Dword
Dim J As Byte
Dim Ad_s As Dword
Dim Ad_f As Dword

'---- Subroutines ----
Declare Sub Char0(byval X As Byte)
Declare Sub Char1(byval X As Byte)
Declare Sub Char2(byval X As Byte)
Declare Sub Char3(byval X As Byte)
Declare Sub Char4(byval X As Byte)
Declare Sub Char5(byval X As Byte)
Declare Sub Char6(byval X As Byte)
Declare Sub Char7(byval X As Byte)
Declare Sub Char8(byval X As Byte)
Declare Sub Char9(byval X As Byte)
Declare Sub Minus(byval X As Byte)

Declare Sub Next_station
Declare Sub Previous_station
Declare Sub Tea5767_wr
Declare Sub Tea5767_mute

Declare Sub Set_rtc
Declare Sub Alarm
Declare Sub Stop_alarm
Declare Sub Show_date_and_temp
Declare Sub Show_alarm_details
Declare Sub Sensors_autodetect
Declare Sub Ds_temp(byval Sensor_nr As Byte)

Declare Sub Send_list
Declare Sub Default_stations
Declare Sub Load_radio_stations

Declare Sub Test_stations

I2cinit
Enable Interrupts

'---- Begin ----
For Mycounter = 1 To 20
   Toggle Led
   Waitms 50
Next
Reset Led

Cls
Waitms 100

Call Sensors_autodetect

'For DEBUG
'Call Test_stations
'Waitms 200

Call Load_radio_stations

R_station = 9                                               'number of frequencies used
Radio_station = 1                                           'Start with the Radion Station, number 1

Radio = 0
Call Tea5767_mute

Reset Lcd_bkl

'For DEBUG
'assigning the time will call the SetTime routine
'Time$ = "00:00:00"
'Date$ = "04/25/18"

Pos1 = 1                                                    'first char's position
Pos2 = 5                                                    '2nd char's position
Pos3 = 10                                                   '3rd char's position
Pos4 = 14                                                   '4th char's position

'For DEBUG
'Time$ = "09:00:58"
'Alarm_time = "09:01"
'Al_type = 2
'Set Alarm_is_set
'Repeat_alarm = 3

Set Lcd_bkl
Timeout = 0
Timeout_sec = 0
Start Timer0

Do
   Gosub Getdatetime
   Hour = Str(_hour)
   Minute = Str(_min)
   Second = Str(_sec)
   Hh = _hour
   Mm = _min


   If Len(hour) = 1 Then Hour = "0" + Hour
   If Len(minute) = 1 Then Minute = "0" + Minute

   H1 = Left(hour , 1)
   H2 = Right(hour , 1)
   M1 = Left(minute , 1)
   M2 = Right(minute , 1)

   Al_hh_str = Left(alarm_time , 2)
   Al_mm_str = Right(alarm_time , 2)

   'Waitms 500

   If Al_hh_str = Hour And Al_mm_str = Minute And Alarm_is_set = 1 Then
      If Once = 0 Then
         Set Led
         Set Once
         Set Alarm_flag
         Repeat_alarm = 3
         Timeout2 = 0
         Timeout2_sec = 0

         Timeout_alarm_off = 0
         Start Timer2
      End If
   End If

   'once reset in timer2 when stopping and when long snooze press

   If Len(second) = 1 Then
      Locate 4 , 19 : Lcd "0" ; _sec
   Else
      Locate 4 , 19 : Lcd _sec
   End If

   If Len(hour) = 1 Then Hour = "0" + Hour
   If Len(minute) = 1 Then Minute = "0" + Minute

   H1 = Left(hour , 1)
   H2 = Right(hour , 1)
   M1 = Left(minute , 1)
   M2 = Right(minute , 1)

   Select Case H1
      Case "0" : Call Char0(pos1)
      Case "1" : Call Char1(pos1)
      Case "2" : Call Char2(pos1)
   End Select

   Select Case H2
      Case "0" : Call Char0(pos2)
      Case "1" : Call Char1(pos2)
      Case "2" : Call Char2(pos2)
      Case "3" : Call Char3(pos2)
      Case "4" : Call Char4(pos2)
      Case "5" : Call Char5(pos2)
      Case "6" : Call Char6(pos2)
      Case "7" : Call Char7(pos2)
      Case "8" : Call Char8(pos2)
      Case "9" : Call Char9(pos2)
   End Select

   Select Case M1
      Case "0" : Call Char0(pos3)
      Case "1" : Call Char1(pos3)
      Case "2" : Call Char2(pos3)
      Case "3" : Call Char3(pos3)
      Case "4" : Call Char4(pos3)
      Case "5" : Call Char5(pos3)
      Case "6" : Call Char6(pos3)
      Case "7" : Call Char7(pos3)
      Case "8" : Call Char8(pos3)
      Case "9" : Call Char9(pos3)
   End Select

   Select Case M2
      Case "0" : Call Char0(pos4)
      Case "1" : Call Char1(pos4)
      Case "2" : Call Char2(pos4)
      Case "3" : Call Char3(pos4)
      Case "4" : Call Char4(pos4)
      Case "5" : Call Char5(pos4)
      Case "6" : Call Char6(pos4)
      Case "7" : Call Char7(pos4)
      Case "8" : Call Char8(pos4)
      Case "9" : Call Char9(pos4)
   End Select

   Locate 2 , 9 : Lcd "."
   Locate 3 , 9 : Lcd "."

   If Alarm_is_set = 1 Then
      Locate 1 , 19 : Lcd "AL"
   Else
      Locate 1 , 19 : Lcd "  "
   End If

   Waitms 750
   Locate 2 , 9 : Lcd " "
   Locate 3 , 9 : Lcd " "

   Waitms 250

   If Alarm_flag = 1 Then Call Alarm

   If Snooze_key = 0 And Alarm_flag = 0 Then
      Set Lcd_bkl
      Timeout = 0
      Timeout_sec = 0
      Start Timer0
   End If

   If Radio_key = 0 Then
      If Do_once = 0 Then
         Set Lcd_bkl
         Timeout = 0
         Timeout_sec = 0
         Start Timer0
         Set Do_once
         If Set_key = 1 Then

            Toggle Radio
            If Radio = 1 Then
               Cls
               Locate 1 , 1 : Lcd " RADIO ON           "
               Waitms 1000
               Call Tea5767_wr
               Waitms 2500
               Cls
            Else
               Cls
               Locate 3 , 1 : Lcd "  >>  RADIO OFF     "
               Call Tea5767_mute
               Waitms 2000
               Cls
            End If
         Else
            Toggle Lcd_on
            If Lcd_on = 1 Then
               Set Lcd_bkl
               Cls
               Locate 2 , 1 : Lcd " >>  LCD Always ON  "
               Waitms 2000
               Cls
            Else
               Cls
               Locate 2 , 1 : Lcd " >>  LCD Light Off  "
               Waitms 2000
               Cls
            End If
         End If
      End If
   Else
      Reset Do_once
   End If

   If Radio = 1 And Plus_key = 0 And Alarm_flag = 0 Then
      Set Lcd_bkl
      Timeout = 0
      Timeout_sec = 0
      Start Timer0
      Incr Radio_station
      If Radio_station > R_station Then Radio_station = 1
      Cls
      Call Tea5767_wr
      Waitms 1500
      Cls
   End If

   If Radio = 1 And Minus_key = 0 And Alarm_flag = 0 Then
      Set Lcd_bkl
      Timeout = 0
      Timeout_sec = 0
      Start Timer0
      Decr Radio_station
      If Radio_station < 1 Then Radio_station = R_station
      Cls
      Call Tea5767_wr
      Waitms 1500
      Cls
   End If

   If Radio = 0 And Plus_key = 0 And Alarm_flag = 0 Then
      Set Lcd_bkl
      Stop Timer0
      Call Show_date_and_temp
      Cls
      Timeout = 0
      Timeout_sec = 0
      Start Timer0
   End If

   If Radio = 0 And Minus_key = 0 And Alarm_flag = 0 Then
      Set Lcd_bkl
      Stop Timer0
      Call Show_alarm_details
      Cls
      Timeout = 0
      Timeout_sec = 0
      Start Timer0
   End If

   If Radio = 1 And Tune_key = 0 And Alarm_flag = 0 Then
      Set Lcd_bkl
      Sound Speaker , 80 , 800

      Cls
      Locate 1 , 1 : Lcd " -  Fine  Tuning  - "
      Locate 4 , 1 : Lcd "<       SAVE       >"
      Call Tea5767_wr

      Exit_var = 0
      While Exit_var = 0
         If Plus_key = 0 Then
            Set Led
            Radio_freq(radio_station) = Radio_freq(radio_station) + 0.005
            F(radio_station) = Fusing(radio_freq(radio_station) , "#.###")
            Call Tea5767_wr
            Waitms 100
         End If

         If Minus_key = 0 Then
            Set Led
            Radio_freq(radio_station) = Radio_freq(radio_station) - 0.005
            F(radio_station) = Fusing(radio_freq(radio_station) , "#.###")
            Call Tea5767_wr
            Waitms 100
         End If

         If Set_key = 0 Then
            Cls
            J = Radio_station * 16
            Ad_f = &H090 + J
            'F(radio_station) = Fusing(radio_freq(radio_station) , "#.###")
            Writeeeprom F(radio_station) , Ad_f
            Call Tea5767_wr
            Sound Speaker , 80 , 800
            Waitms 1500
            Exit_var = 1
         End If
         Reset Led
      Wend
      Timeout = 0
      Timeout_sec = 0
      Start Timer0
   End If

   If Radio = 0 And Tune_key = 0 And Alarm_flag = 0 Then
      Set Lcd_bkl
      Sound Speaker , 80 , 800
      Cls
      Locate 1 , 1 : Lcd " -  UART Utility  - "
      Locate 4 , 1 : Lcd "        EXIT        "
      Enable Serial
      Exit_var = 0
      While Exit_var = 0
         If Rx_flag = 1 Then
            Reset Rx_flag
            Rx_header = Left(rx_str , 1)
            If Rx_header = "$" Then
               S_pos = Mid(rx_str , 2 , 1)
               Station_pos = Val(s_pos)
               At_pos = Instr(rx_str , "@")
               Name_chars = At_pos - 4
               Freq_chars = Len(rx_str) - At_pos

               'first remove the current entry's content (name and frequency) from EEPROM
               J = Station_pos * 16
               Ad_s = &H000 + J
               Ad_f = &H090 + J

               Station(station_pos) = Chr(0)
               F(station_pos) = Chr(0)
               Radio_freq(station_pos) = 0
               For I = 0 To 15
                  Address = Ad_s + I
                  Writeeeprom Station(station_pos) , Address
                  Address = Ad_f + I
                  Writeeeprom F(station_pos) , Address
               Next

               'if the new values are different than "empty" write them to EEPROM
               If Name_chars > 0 Then
                  Station(station_pos) = Mid(rx_str , 4 , Name_chars)
                  Writeeeprom Station(station_pos) , Ad_s
               End If
               If Freq_chars > 0 Then
                  F(station_pos) = Right(rx_str , Freq_chars)
                  Radio_freq(station_pos) = Val(f(station_pos))
                  Writeeeprom F(station_pos) , Ad_f
               End If
            End If

            If Rx_str = "#send!" Then
               Call Send_list
               Locate 2 , 1 : Lcd "        List Sent ! "
               Waitms 200
            End If

            If Rx_str = "!sent" Then
               Print "!received"
               Locate 2 , 1 : Lcd "    List Received ! "
               Waitms 200
            End If

             If Rx_str = "#clrlst!" Then
              'erase all the entries (name and frequency) from EEPROM
               For Station_pos = 1 To 9
                  J = Station_pos * 16
                  Ad_s = &H000 + J
                  Ad_f = &H090 + J

                  Station(station_pos) = Chr(0)
                  F(station_pos) = Chr(0)
                  For I = 0 To 15
                     Address = Ad_s + I
                     Writeeeprom Station(station_pos) , Address
                     Address = Ad_f + I
                     Writeeeprom F(station_pos) , Address
                     Radio_freq(station_pos) = 0
                  Next
               Next
               Print "!erased"
               Locate 2 , 1 : Lcd "      List Erased ! "
               Waitms 200
             End If
         End If

         If Set_key = 0 Then
            Cls
            Set Led
            Sound Speaker , 80 , 800
            Waitms 1500
            Exit_var = 1
         End If
         Reset Led
      Wend
      Disable Serial
      Timeout = 0
      Timeout_sec = 0
      Start Timer0
   End If

   If Set_key = 0 Then
      Set Lcd_bkl
      Timeout = 0
      Timeout_sec = 0
      Start Timer0
      Incr Set_key_time
      If Radio = 0 And Do_once = 0 And Set_key_time >= 3 Then
         Set Do_once
         Call Set_rtc
         Set_key_time = 0
      End If
   Else
      Reset Do_once
      Set_key_time = 0
   End If

   If Timeout_flag = 1 Then
      Reset Timeout_flag
      Stop Timer0
      If Lcd_on = 0 Then Reset Lcd_bkl
   End If

   If Snooze_long_press = 1 And Do_once = 0 Then
      If Repeat_alarm > 0 Then
         Repeat_alarm = 0
         Alarm_flag = 0
         Call Stop_alarm
         Stop Timer2
      End If
      Reset Snooze_long_press
      Set Do_once
      Sound Speaker , 500 , 1000
      Toggle Alarm_is_set
      If Alarm_is_set = 1 Then Repeat_alarm = 3
   End If
Loop

End


Serial0bytereceived:
   Pushall
   Dim A_chr As Byte
   If Ischarwaiting(#1) > 0 Then
      A_chr = Inkey(#1)
      If A_chr <> 10 And A_chr <> 13 Then
         Rxvalue = Rxvalue + Chr(a_chr)
      End If
   End If
   If A_chr = 10 Then
      Set Rx_flag
      Rx_str = Rxvalue
      Rxvalue = ""
   End If
   Popall
Return

Timer0_isr:
   Timer0 = Timer0_reload
   Incr Timeout
   If Timeout >= 100 Then                                   '1 second passed
      Timeout = 0
      Incr Timeout_sec
   End If
   If Timeout_sec >= 5 Then
      Timeout_sec = 0
      Set Timeout_flag
   End If

   If Snooze_key = 0 And Snooze_long_press = 0 Then
      Incr Snooze_press_time
      If Snooze_press_time >= 200 Then Set Snooze_long_press
   Else
      Snooze_press_time = 0
   End If
Return

Timer2_isr:
   Timer2 = Timer2_reload
   Incr Timeout2
   Incr Timeout3

   If Snooze_key = 0 And Onetime = 0 And Alarm_flag = 1 Then
      Onetime = 1
      Timeout2_sec = 0
      Reset T2_3min_flag
      Reset Alarm_flag
      Call Stop_alarm
   End If

   If Timeout3 >= 200 Then
      Timeout3 = 0
      If Onetime = 1 Then Reset Onetime
   End If

   If Timeout2 >= 100 Then                                  '1 second passed
      Timeout2 = 0
      Incr Timeout2_sec
      Incr Timeout_alarm_off
   End If

   If Timeout2_sec >= 180 Then                              'sleep 3 more minutes (SNOOZE)
      Set T2_3min_flag
      Timeout2_sec = 0
   End If

   If Repeat_alarm > 0 Then
      If T2_3min_flag = 1 And Alarm_flag = 0 Then
         Reset T2_3min_flag
         Set Alarm_flag
         Decr Repeat_alarm
      End If
   Else
      Repeat_alarm = 0
      Reset Alarm_flag
      Reset Once
      Call Stop_alarm
      Stop Timer2
   End If

   If Timeout_alarm_off = 600 Then                          'ten minutes
      Repeat_alarm = 0
      Reset Alarm_flag
      Reset Once
      Call Stop_alarm
      Stop Timer2
   End If
Return

'called from ds1307clock.lib
Getdatetime:
  I2cstart                                                  ' Generate start code
  I2cwbyte Ds1307w                                          ' send address
  I2cwbyte 0                                                ' start address in 1307

  I2cstart                                                  ' Generate start code
  I2cwbyte Ds1307r                                          ' send address
  I2crbyte _sec , Ack
  I2crbyte _min , Ack                                       ' minute
  I2crbyte _hour , Ack                                      ' hour
  I2crbyte Weekday , Ack                                    ' Day of Week
  I2crbyte _day , Ack                                       ' Day of Month
  I2crbyte _month , Ack                                     ' Month of Year
  I2crbyte _year , Nack                                     ' Year
  I2cstop
  _sec = Makedec(_sec) : _min = Makedec(_min) : _hour = Makedec(_hour)
  _day = Makedec(_day) : _month = Makedec(_month) : _year = Makedec(_year)
Return

Setdate:
  _day = Makebcd(_day) : _month = Makebcd(_month) : _year = Makebcd(_year)
  I2cstart                                                  ' Generate start code
  I2cwbyte Ds1307w                                          ' send address
  I2cwbyte 4                                                ' starting address in 1307
  I2cwbyte _day                                             ' Send Data to second
  I2cwbyte _month                                           ' minute
  I2cwbyte _year                                            ' hour
  I2cstop
Return

Settime:
  _sec = Makebcd(_sec) : _min = Makebcd(_min) : _hour = Makebcd(_hour)
  I2cstart                                                  ' Generate start code
  I2cwbyte Ds1307w                                          ' send address
  I2cwbyte 0                                                ' starting address in 1307
  I2cwbyte _sec                                             ' Send Data to second
  I2cwbyte _min                                             ' minute
  I2cwbyte _hour                                            ' hour
  I2cstop
Return

Sub Char0(byval X As Byte)
   Locate 1 , X : Lcd Chr(0) ; Chr(2) ; Chr(3) ; Chr(6)
   Locate 2 , X : Lcd Chr(1) ; "  " ; Chr(7)
   Locate 3 , X : Lcd Chr(0) ; "  " ; Chr(6)
   Locate 4 , X : Lcd Chr(1) ; Chr(4) ; Chr(5) ; Chr(7)
End Sub

Sub Char1(byval X As Byte)
   Locate 1 , X : Lcd "  " ; Chr(6) ; " "
   Locate 2 , X : Lcd "  " ; Chr(7) ; " "
   Locate 3 , X : Lcd "  " ; Chr(6) ; " "
   Locate 4 , X : Lcd "  " ; Chr(7) ; " "
End Sub

Sub Char2(byval X As Byte)
   Locate 1 , X : Lcd " " ; Chr(2) ; Chr(3) ; Chr(6)
   Locate 2 , X : Lcd " " ; Chr(4) ; Chr(5) ; Chr(7)
   Locate 3 , X : Lcd Chr(0) ; "   "
   Locate 4 , X : Lcd Chr(1) ; Chr(4) ; Chr(5) ; " "
End Sub

Sub Char3(byval X As Byte)
   Locate 1 , X : Lcd " " ; Chr(2) ; Chr(3) ; Chr(6)
   Locate 2 , X : Lcd " " ; Chr(4) ; Chr(5) ; Chr(7)
   Locate 3 , X : Lcd "   " ; Chr(6)
   Locate 4 , X : Lcd " " ; Chr(4) ; Chr(5) ; Chr(7)
End Sub

Sub Char4(byval X As Byte)
   Locate 1 , X : Lcd Chr(0) ; "  " ; Chr(6)
   Locate 2 , X : Lcd Chr(1) ; Chr(4) ; Chr(5) ; Chr(7)
   Locate 3 , X : Lcd "   " ; Chr(6)
   Locate 4 , X : Lcd "   " ; Chr(7)
End Sub

Sub Char5(byval X As Byte)
   Locate 1 , X : Lcd Chr(0) ; Chr(2) ; Chr(3) ; " "
   Locate 2 , X : Lcd Chr(1) ; Chr(4) ; Chr(5) ; " "
   Locate 3 , X : Lcd "   " ; Chr(6)
   Locate 4 , X : Lcd " " ; Chr(4) ; Chr(5) ; Chr(7)
End Sub

Sub Char6(byval X As Byte)
   Locate 1 , X : Lcd Chr(0) ; Chr(2) ; Chr(3) ; " "
   Locate 2 , X : Lcd Chr(1) ; Chr(4) ; Chr(5) ; " "
   Locate 3 , X : Lcd Chr(0) ; "  " ; Chr(6)
   Locate 4 , X : Lcd Chr(1) ; Chr(4) ; Chr(5) ; Chr(7)
End Sub

Sub Char7(byval X As Byte)
   Locate 1 , X : Lcd " " ; Chr(2) ; Chr(3) ; Chr(6)
   Locate 2 , X : Lcd "   " ; Chr(7)
   Locate 3 , X : Lcd "   " ; Chr(6)
   Locate 4 , X : Lcd "   " ; Chr(7)
End Sub

Sub Char8(byval X As Byte)
   Locate 1 , X : Lcd Chr(0) ; Chr(2) ; Chr(3) ; Chr(6)
   Locate 2 , X : Lcd Chr(1) ; Chr(4) ; Chr(5) ; Chr(7)
   Locate 3 , X : Lcd Chr(0) ; "  " ; Chr(6)
   Locate 4 , X : Lcd Chr(1) ; Chr(4) ; Chr(5) ; Chr(7)
End Sub

Sub Char9(byval X As Byte)
   Locate 1 , X : Lcd Chr(0) ; Chr(2) ; Chr(3) ; Chr(6)
   Locate 2 , X : Lcd Chr(1) ; Chr(4) ; Chr(5) ; Chr(7)
   Locate 3 , X : Lcd "   " ; Chr(6)
   Locate 4 , X : Lcd " " ; Chr(4) ; Chr(5) ; Chr(7)
End Sub

Sub Minus(byval X As Byte)
   Locate 1 , X : Lcd "   "
   Locate 2 , X : Lcd "   "
   Locate 3 , X : Lcd Chr(7) ; Chr(7) ; " "
   Locate 4 , X : Lcd "   "
End Sub

Sub Tea5767_wr
   Pll = Radio_freq(radio_station) * 1000000
   Pll = Pll + Frequency
   Pll = Pll * 4
   Pll = Pll / Module_frequency
   Pll_freq = Int(pll)
   1st = High(pll_freq)
   Reset 1st.6                                              'no Searching
   Reset 1st.7                                              'No Mute
   2nd = Low(pll_freq)
   3rd = &B00010000
   4th = &B00010010
   5th = &B00000000
   I2cstart
   I2cwbyte Tea5767w
   I2cwbyte 1st
   I2cwbyte 2nd
   I2cwbyte 3rd
   I2cwbyte 4th
   I2cwbyte 5th
   I2cstop
   If Len(station(radio_station)) <> 0 Then
      Locate 2 , 1 : Lcd Radio_station ; ".                  "
      Locate 2 , 4 : Lcd Station(radio_station)
   Else
      Locate 2 , 1 : Lcd Radio_station ; ".                  "
      Locate 2 , 4 : Lcd "no name"
   End If

   'W = Fusing(radio_freq(radio_station) , "#.###")          'output formatting

   If Radio_freq(radio_station) < 76 Or Radio_freq(radio_station) > 108 Then
      Locate 3 , 1 : Lcd "                    "
      Locate 3 , 1 : Lcd "   no frequency"
   Else
      Locate 3 , 1 : Lcd "                    "
      Locate 3 , 4 : Lcd F(radio_station) ; " MHz"
   End If
End Sub

Sub Tea5767_mute
   Pll = Radio_freq(radio_station) * 1000000
   Pll = Pll + Frequency
   Pll = Pll * 4
   Pll = Pll / Module_frequency
   Pll_freq = Int(pll)
   1st = High(pll_freq)
   Reset 1st.6                                              'no Searching
   Set 1st.7                                                'Mute
   2nd = Low(pll_freq)
   3rd = &B00010000
   4th = &B00010010
   5th = &B00000000
   I2cstart
   I2cwbyte Tea5767w
   I2cwbyte 1st
   I2cwbyte 2nd
   I2cwbyte 3rd
   I2cwbyte 4th
   I2cwbyte 5th
   I2cstop
End Sub

Sub Set_rtc
   Cls
   Reset Exit_menu
   Locate 4 , 1 : Lcd "<      CHOOSE      >"
   Stop Timer0
   Rtc_menu = 1
   Locate 1 , 1 : Lcd "1. Enter new Time   "
   Exit_var = 0
   Sound Speaker , 100 , 3000
   Waitms 1000
   While Exit_var = 0
      If Plus_key = 0 Then
         Incr Rtc_menu
         If Rtc_menu > 5 Then Rtc_menu = 1
         Select Case Rtc_menu
            Case 1:
               Locate 1 , 1 : Lcd "1. Enter new Time   "
            Case 2:
               Locate 1 , 1 : Lcd "2. Enter new Date   "
            Case 3:
               Locate 1 , 1 : Lcd "3. Set Alarm Time   "
            Case 4:
               Locate 1 , 1 : Lcd "4. Alarm Options    "
            Case 5:
               Locate 1 , 1 : Lcd "5. Exit Config Menu "
         End Select
         Waitms 300
      End If

      If Minus_key = 0 Then
         Decr Rtc_menu
         If Rtc_menu < 1 Then Rtc_menu = 5
         Select Case Rtc_menu
            Case 1:
               Locate 1 , 1 : Lcd "1. Enter new Time   "
            Case 2:
               Locate 1 , 1 : Lcd "2. Enter new Date   "
            Case 3:
               Locate 1 , 1 : Lcd "3. Set Alarm Time   "
            Case 4:
               Locate 1 , 1 : Lcd "4. Alarm Options    "
            Case 5:
               Locate 1 , 1 : Lcd "5. Exit Config Menu "

         End Select
         Waitms 300
      End If

      If Set_key = 0 Then
         Cls
         Select Case Rtc_menu
            Case 1:
               Sound Speaker , 50 , 1000
               Locate 1 , 1 : Lcd "1. Enter new Time   "
               Locate 2 , 1 : Lcd "      HH:MM:SS      "
               Locate 4 , 1 : Lcd "<        OK        >"

               Gosub Getdatetime
               Hh = _hour
               Mm = _min
               Ss = _sec
               Hour = Str(hh)
               Minute = Str(mm)
               Second = Str(ss)
               If Len(hour) = 1 Then
                  Locate 3 , 7 : Lcd "0" ; Hour
               Else
                  Locate 3 , 7 : Lcd Hour
               End If

               While Exit_var = 0
                  Locate 2 , 7 : Lcd "HH"
                  Waitms 250
                  Locate 2 , 7 : Lcd "  "
                  Waitms 100
                  If Plus_key = 0 And Hh < 23 Then
                     Incr Hh

                     Hour = Str(hh)
                     If Len(hour) = 1 Then
                        Locate 3 , 7 : Lcd "0" ; Hour
                     Else
                        Locate 3 , 7 : Lcd Hour
                     End If
                  End If

                  If Minus_key = 0 And Hh > 0 Then
                     Decr Hh

                     Hour = Str(hh)
                     If Len(hour) = 1 Then
                        Locate 3 , 7 : Lcd "0" ; Hour
                     Else
                        Locate 3 , 7 : Lcd Hour
                     End If
                  End If

                  If Set_key = 0 Then
                     Locate 3 , 9 : Lcd ":"
                     Exit_var = 1
                     Waitms 200
                  End If
               Wend
               Locate 2 , 7 : Lcd "HH"
               Exit_var = 0
               If Len(minute) = 1 Then
                  Locate 3 , 10 : Lcd "0" ; Minute
               Else
                  Locate 3 , 10 : Lcd Minute
               End If

               While Exit_var = 0
                  Locate 2 , 10 : Lcd "MM"
                  Waitms 250
                  Locate 2 , 10 : Lcd "  "
                  Waitms 100
                  If Plus_key = 0 And Mm < 59 Then
                     Incr Mm

                     Minute = Str(mm)
                     If Len(minute) = 1 Then
                        Locate 3 , 10 : Lcd "0" ; Minute
                     Else
                        Locate 3 , 10 : Lcd Minute
                     End If
                  End If

                  If Minus_key = 0 And Mm > 0 Then
                     Decr Mm

                     Minute = Str(mm)
                     If Len(minute) = 1 Then
                        Locate 3 , 10 : Lcd "0" ; Minute
                     Else
                        Locate 3 , 10 : Lcd Minute
                     End If
                  End If

                  If Set_key = 0 Then
                     Locate 3 , 12 : Lcd ":"
                     Exit_var = 1
                     Waitms 200
                  End If
               Wend

               Locate 2 , 10 : Lcd "MM"
               Exit_var = 0
               If Len(second) = 1 Then
                  Locate 3 , 13 : Lcd "0" ; Second
               Else
                  Locate 3 , 13 : Lcd Second
               End If

               While Exit_var = 0
                  Locate 2 , 13 : Lcd "SS"
                  Waitms 250
                  Locate 2 , 13 : Lcd "  "
                  Waitms 100
                  If Plus_key = 0 And Ss < 59 Then
                     Incr Ss

                     Second = Str(ss)
                     If Len(second) = 1 Then
                        Locate 3 , 13 : Lcd "0" ; Second
                     Else
                        Locate 3 , 13 : Lcd Second
                     End If
                  End If

                  If Minus_key = 0 And Ss > 0 Then
                     Decr Ss

                     Second = Str(ss)
                     If Len(second) = 1 Then
                        Locate 3 , 13 : Lcd "0" ; Second
                     Else
                        Locate 3 , 13 : Lcd Second
                     End If
                  End If

                  If Set_key = 0 Then
                     Exit_var = 1
                     Sound Speaker , 50 , 2000
                     If Len(hour) = 1 Then Hour = "0" + Hour
                     If Len(minute) = 1 Then Minute = "0" + Minute
                     If Len(second) = 1 Then Second = "0" + Second

                     Newtime = Hour + ":"
                     Newtime = Newtime + Minute
                     Newtime = Newtime + ":"
                     Newtime = Newtime + Second
                     Time$ = Newtime
                  End If
               Wend

               Set Exit_menu
               Cls
               Waitms 500

            Case 2:
               Sound Speaker , 50 , 1000
               Locate 1 , 1 : Lcd "2. Enter new Date   "
               Locate 2 , 1 : Lcd "      MM/DD/YY      "
               Locate 4 , 1 : Lcd "<        OK        >"

               Gosub Getdatetime
               Dd = _day
               Mn = _month
               Yy = _year
               Day = Str(dd)
               Month = Str(mn)
               Year = Str(yy)
               Temp_var = Yy Mod 4

               If Temp_var = 0 Then
                  Set Is_leap_year
               Else
                  Reset Is_leap_year
               End If

               If Len(month) = 1 Then
                  Locate 3 , 7 : Lcd "0" ; Month
               Else
                  Locate 3 , 7 : Lcd Month
               End If

               While Exit_var = 0
                  Locate 2 , 7 : Lcd "MM"
                  Waitms 250
                  Locate 2 , 7 : Lcd "  "
                  Waitms 100
                  If Plus_key = 0 And Mn < 12 Then
                     Incr Mn
                     Month = Str(mn)
                     If Len(month) = 1 Then
                        Locate 3 , 7 : Lcd "0" ; Month
                     Else
                        Locate 3 , 7 : Lcd Month
                     End If
                  End If

                  If Minus_key = 0 And Mn > 1 Then
                     Decr Mn
                     Month = Str(mn)
                     If Len(month) = 1 Then
                        Locate 3 , 7 : Lcd "0" ; Month
                     Else
                        Locate 3 , 7 : Lcd Month
                     End If
                  End If

                  If Set_key = 0 Then
                     Locate 3 , 9 : Lcd "/"
                     Exit_var = 1
                     Waitms 200
                  End If
               Wend
               Locate 2 , 7 : Lcd "MM"
               Exit_var = 0

               If Len(day) = 1 Then
                  Locate 3 , 10 : Lcd "0" ; Day
               Else
                  Locate 3 , 10 : Lcd Day
               End If

               While Exit_var = 0
                  Locate 2 , 10 : Lcd "DD"
                  Waitms 250
                  Locate 2 , 10 : Lcd "  "
                  Waitms 100
                  If Plus_key = 0 Then
                     If Mn = 1 Or Mn = 3 Or Mn = 5 Or Mn = 7 Or Mn = 8 Or Mn = 10 Or Mn = 12 Then
                        If Dd < 31 Then
                           Incr Dd
                           Day = Str(dd)
                           If Len(day) = 1 Then
                              Locate 3 , 10 : Lcd "0" ; Day
                           Else
                              Locate 3 , 10 : Lcd Day
                           End If
                        End If
                     End If

                     If Mn = 4 Or Mn = 6 Or Mn = 9 Or Mn = 11 Then
                        If Dd < 30 Then
                           Incr Dd
                           Day = Str(dd)
                           If Len(day) = 1 Then
                              Locate 3 , 10 : Lcd "0" ; Day
                           Else
                              Locate 3 , 10 : Lcd Day
                           End If
                        End If
                     End If

                     If Mn = 2 Then
                        If Is_leap_year = 1 Then
                           If Dd < 29 Then
                              Incr Dd
                              Day = Str(dd)
                              If Len(day) = 1 Then
                                 Locate 3 , 10 : Lcd "0" ; Day
                              Else
                                 Locate 3 , 10 : Lcd Day
                              End If
                           End If
                        Else
                           If Dd < 28 Then
                              Incr Dd

                              Day = Str(dd)
                              If Len(day) = 1 Then
                                 Locate 3 , 10 : Lcd "0" ; Day
                              Else
                                 Locate 3 , 10 : Lcd Day
                              End If
                           End If
                        End If
                     End If
                  End If

                  If Minus_key = 0 And Dd > 1 Then
                     Decr Dd
                     Day = Str(dd)
                     If Len(day) = 1 Then
                        Locate 3 , 10 : Lcd "0" ; Day
                     Else
                        Locate 3 , 10 : Lcd Day
                     End If
                  End If

                  If Set_key = 0 Then
                     Locate 3 , 12 : Lcd "/"
                     Exit_var = 1
                     Waitms 200
                  End If
               Wend

               Locate 2 , 10 : Lcd "DD"
               Exit_var = 0

               If Len(year) = 1 Then
                  Locate 3 , 13 : Lcd "0" ; Year
               Else
                  Locate 3 , 13 : Lcd Year
               End If

               While Exit_var = 0
                  Locate 2 , 13 : Lcd "YY"
                  Waitms 250
                  Locate 2 , 13 : Lcd "  "
                  Waitms 100
                  If Plus_key = 0 And Yy < 80 Then
                     Incr Yy
                     Year = Str(yy)
                     If Len(year) = 1 Then
                        Locate 3 , 13 : Lcd "0" ; Year
                     Else
                        Locate 3 , 13 : Lcd Year
                     End If
                  End If

                  If Minus_key = 0 And Yy > 0 Then
                     Decr Yy
                     Year = Str(yy)
                     If Len(year) = 1 Then
                        Locate 3 , 13 : Lcd "0" ; Year
                     Else
                        Locate 3 , 13 : Lcd Year
                     End If
                  End If

                  If Set_key = 0 Then
                     Exit_var = 1
                     Sound Speaker , 50 , 2000
                     If Len(month) = 1 Then Month = "0" + Month
                     If Len(day) = 1 Then Day = "0" + Day
                     If Len(year) = 1 Then Year = "0" + Year
                     Newdate = Month + "/"
                     Newdate = Newdate + Day
                     Newdate = Newdate + "/"
                     Newdate = Newdate + Year
                     Date$ = Newdate
                  End If
               Wend
               Set Exit_menu
               Cls
               Waitms 500

            Case 3:
               Sound Speaker , 50 , 1000
               Locate 1 , 1 : Lcd "3. Set Alarm Time   "
               Locate 2 , 1 : Lcd "        HH:MM       "
               Locate 4 , 1 : Lcd "<        OK        >"
               Al_hh_str = Left(alarm_time , 2)
               Al_mm_str = Right(alarm_time , 2)
               Al_hh = Val(al_hh_str)
               Al_mm = Val(al_mm_str)
               If Al_hh < 10 Then
                  Locate 3 , 9 : Lcd "0" ; Al_hh
               Else
                  Locate 3 , 9 : Lcd Al_hh
               End If

               While Exit_var = 0
                  Locate 2 , 9 : Lcd "HH"
                  Waitms 200
                  Locate 2 , 9 : Lcd "  "
                  Waitms 80
                  If Plus_key = 0 And Al_hh < 23 Then
                     Incr Al_hh
                     If Al_hh < 10 Then
                        Locate 3 , 9 : Lcd "0" ; Al_hh
                     Else
                        Locate 3 , 9 : Lcd Al_hh
                     End If
                  End If

                  If Minus_key = 0 And Al_hh > 0 Then
                     Decr Al_hh
                     If Al_hh < 10 Then
                        Locate 3 , 9 : Lcd "0" ; Al_hh
                     Else
                        Locate 3 , 9 : Lcd Al_hh
                     End If
                  End If

                  If Set_key = 0 Then
                     Locate 3 , 11 : Lcd ":"
                     Exit_var = 1
                     Waitms 200
                  End If
               Wend
               Locate 2 , 9 : Lcd "HH"
               Exit_var = 0
               If Al_mm < 10 Then
                  Locate 3 , 12 : Lcd "0" ; Al_mm
               Else
                  Locate 3 , 12 : Lcd Al_mm
               End If

               While Exit_var = 0
                  Locate 2 , 12 : Lcd "MM"
                  Waitms 200
                  Locate 2 , 12 : Lcd "  "
                  Waitms 80
                  If Plus_key = 0 And Mm < 59 Then
                     Incr Al_mm
                     Minute = Str(mm)
                     If Al_mm < 10 Then
                        Locate 3 , 12 : Lcd "0" ; Al_mm
                     Else
                        Locate 3 , 12 : Lcd Al_mm
                     End If
                  End If

                  If Minus_key = 0 And Al_mm > 0 Then
                     Decr Al_mm
                     If Al_mm < 10 Then
                        Locate 3 , 12 : Lcd "0" ; Al_mm
                     Else
                        Locate 3 , 12 : Lcd Al_mm
                     End If
                  End If

                  If Set_key = 0 Then
                     Exit_var = 1
                     Sound Speaker , 50 , 2000
                     If Al_hh < 10 Then
                        Alarm_time = "0" + Str(al_hh)
                     Else
                        Alarm_time = Str(al_hh)
                     End If
                     Alarm_time = Alarm_time + ":"

                     If Al_mm < 10 Then
                        Alarm_time = Alarm_time + "0"
                        Alarm_time = Alarm_time + Str(al_mm)
                     Else
                        Alarm_time = Alarm_time + Str(al_mm)
                     End If

                     Reset Once
                     Set Alarm_is_set
                     Repeat_alarm = 3
                  End If
               Wend
               Set Exit_menu
               Cls
               Waitms 500

            Case 4:
               Cls
               Sound Speaker , 100 , 2000
               Locate 1 , 1 : Lcd "4. Alarm Options    "
               Locate 4 , 1 : Lcd "<        OK        >"
               Alarm_type = Al_type
               Select Case Al_type
                  Case 1:
                     Locate 2 , 1 : Lcd "      Alarm OFF     "
                  Case 2:
                     Locate 2 , 1 : Lcd " Wake up with Radio "
                  Case 3:
                     Locate 2 , 1 : Lcd "    Buzzer Alarm    "
                  Case 4:
                     Locate 2 , 1 : Lcd "     Light only     "
               End Select
               Exit_var = 0

               Waitms 500
               While Exit_var = 0
                  If Plus_key = 0 Then
                     Incr Alarm_type
                     If Alarm_type > 4 Then Alarm_type = 1
                     Select Case Alarm_type
                        Case 1:
                           Locate 2 , 1 : Lcd "      Alarm OFF     "
                           Reset Alarm_is_set
                        Case 2:
                           Locate 2 , 1 : Lcd " Wake up with Radio "
                           Set Alarm_is_set
                        Case 3:
                           Locate 2 , 1 : Lcd "    Buzzer Alarm    "
                           Set Alarm_is_set
                        Case 4:
                           Locate 2 , 1 : Lcd "     Light only     "
                           Set Alarm_is_set
                     End Select
                     Waitms 250
                  End If

                  If Minus_key = 0 Then
                     Decr Alarm_type
                     If Alarm_type < 1 Then Alarm_type = 4
                     Select Case Alarm_type
                        Case 1:
                           Locate 2 , 1 : Lcd "      Alarm OFF     "
                           Reset Alarm_is_set
                        Case 2:
                           Locate 2 , 1 : Lcd " Wake up with Radio "
                           Set Alarm_is_set
                        Case 3:
                           Locate 2 , 1 : Lcd "    Buzzer Alarm    "
                           Set Alarm_is_set
                        Case 4:
                           Locate 2 , 1 : Lcd "     Light only     "
                           Set Alarm_is_set
                     End Select
                     Waitms 250
                  End If

                  If Set_key = 0 Then
                     Exit_var = 1
                     Sound Speaker , 50 , 1000
                     Al_type = Alarm_type
                     If Alarm_is_set = 1 Then
                        Repeat_alarm = 3
                        Reset Once
                     Else
                        Repeat_alarm = 0
                     End If
                  End If
               Wend
               Set Exit_menu
               Cls
               Waitms 500

            Case 5:
               Cls
               Exit_var = 1
               Set Exit_menu
               Sound Speaker , 50 , 1000
         End Select
         Exit_var = 0
         If Exit_menu = 1 Then Exit_var = 1
      End If
   Wend

   Cls
   Set_key_time = 0
   Start Timer0
End Sub

Sub Alarm
   Select Case Al_type
      Case 2:                                               'just turn the RADIO ON
         Cls
         If Radio = 0 Then
            Radio = 1
            Call Tea5767_wr
         End If
         Set Lcd_bkl
         Locate 1 , 1 : Lcd "      WAKE UP !!    "
         Waitms 800
         Cls

      Case 3:
         Set Led
         Set Lcd_bkl
         Cls
         Locate 1 , 1 : Lcd "      WAKE UP !!    "
         Sound Speaker , 400 , 800
         Waitms 25
         Sound Speaker , 400 , 800
         Waitms 100
         Cls

      Case 4:
         Reset Lcd_bkl
         Reset Led
         Waitms 100
         Cls
         Set Lcd_bkl
         Set Led
         Locate 1 , 1 : Lcd "      WAKE UP !!    "
         Waitms 500
         Cls
   End Select
End Sub

Sub Stop_alarm
   If Radio = 1 Then
      Radio = 0
      Call Tea5767_mute
   End If

   Reset Led
   Timeout = 0
   Timeout_sec = 0
   Start Timer0
End Sub

Sub Show_date_and_temp
Local Weekday_str As String * 9
Local Dow As Byte
Local Month_str As String * 9
   Cls
   Gosub Getdatetime
   Dow = Dayofweek()
   Weekday_str = Lookupstr(dow , Weekdays)

   Select Case _month
      Case 1:
         Month_str = "January"
      Case 2:
         Month_str = "February"
      Case 3:
         Month_str = "March"
      Case 4:
         Month_str = "April"
      Case 5:
         Month_str = "May"
      Case 6:
         Month_str = "June"
      Case 7:
         Month_str = "July"
      Case 8:
         Month_str = "August"
      Case 9:
         Month_str = "September"
      Case 10:
         Month_str = "October"
      Case 11:
         Month_str = "November"
      Case 12:
         Month_str = "December"
   End Select

   Locate 1 , 1 : Lcd Weekday_str
   Locate 2 , 1 : Lcd Month_str ; " " ; _day ; ".  20" ; _year ; " "

   Select Case Nr
      Case 0:
         Locate 3 , 1 : Lcd " IN: No Temp. Sensor"
         Locate 4 , 1 : Lcd "OUT: No Temp. Sensor"
      Case 1:
         S_nr = 1
         Call Ds_temp(s_nr)
         Locate 3 , 1 : Lcd " IN  <<  "
         V = Left(tempunit , 1)
         Ts_len = Len(tempunit)
         If V = "-" Then
            Ts_len = Len(tempunit) - 1
            If Ts_len = 2 Then
               Locate 3 , 10 : Lcd Tempunit ; "." ; Tempdecimal ; " 'C  "
            End If
            If Ts_len = 1 Then
               Locate 3 , 11 : Lcd Tempunit ; "." ; Tempdecimal ; " 'C  "
            End If
         Else
            Ts_len = Len(tempunit)
            If Ts_len = 2 Then
               Locate 3 , 11 : Lcd Tempunit ; "." ; Tempdecimal ; " 'C  "
            End If
            If Ts_len = 1 Then
               Locate 3 , 12 : Lcd Tempunit ; "." ; Tempdecimal ; " 'C  "
            End If
         End If
         Locate 4 , 1 : Lcd "OUT: No Temp. Sensor"

      Case 2:
         S_nr = 2
         Call Ds_temp(s_nr)
         Locate 3 , 1 : Lcd " IN  <<  "
         V = Left(tempunit , 1)
         Ts_len = Len(tempunit)
         If V = "-" Then
            Ts_len = Len(tempunit) - 1
            If Ts_len = 2 Then
               Locate 3 , 10 : Lcd Tempunit ; "." ; Tempdecimal ; " 'C  "
            End If
            If Ts_len = 1 Then
               Locate 3 , 11 : Lcd Tempunit ; "." ; Tempdecimal ; " 'C  "
            End If
         Else
            Ts_len = Len(tempunit)
            If Ts_len = 2 Then
               Locate 3 , 11 : Lcd Tempunit ; "." ; Tempdecimal ; " 'C  "
            End If
            If Ts_len = 1 Then
               Locate 3 , 12 : Lcd Tempunit ; "." ; Tempdecimal ; " 'C  "
            End If
         End If

         S_nr = 1
         Call Ds_temp(s_nr)
         Locate 4 , 1 : Lcd " OUT >>  "
         V = Left(tempunit , 1)
         Ts_len = Len(tempunit)
         If V = "-" Then
            Ts_len = Len(tempunit) - 1
            If Ts_len = 2 Then
               Locate 4 , 10 : Lcd Tempunit ; "." ; Tempdecimal ; " 'C  "
            End If
            If Ts_len = 1 Then
               Locate 4 , 11 : Lcd Tempunit ; "." ; Tempdecimal ; " 'C  "
            End If
         Else
            Ts_len = Len(tempunit)
            If Ts_len = 2 Then
               Locate 4 , 11 : Lcd Tempunit ; "." ; Tempdecimal ; " 'C  "
            End If
            If Ts_len = 1 Then
               Locate 4 , 12 : Lcd Tempunit ; "." ; Tempdecimal ; " 'C  "
            End If
         End If
   End Select
   Waitms 5000
End Sub

Sub Show_alarm_details
   Cls
   If Alarm_is_set = 1 Then
      Locate 1 , 1 : Lcd "Alarm is set !"
      Select Case Al_type
         Case 2:
            Locate 2 , 1 : Lcd " Wake up with Radio "
         Case 3:
            Locate 2 , 1 : Lcd "    Buzzer Alarm    "
         Case 4:
            Locate 2 , 1 : Lcd "     Light only     "
      End Select
      Locate 4 , 7 : Lcd "@ " ; Alarm_time
   Else
      Locate 2 , 1 : Lcd "Alarm not set"
      Locate 3 , 7 : Lcd ">> " ; Alarm_time
   End If
   Waitms 5000
End Sub

Sub Sensors_autodetect
   Print "serial activated"
   Set Lcd_bkl
   Restore Fwdat1
   Locate 1 , 2
   For I = 1 To 17
      Read Dat_s
      Decival = Binval(dat_s)
      Lcd Chr(decival)
   Next
   Restore Fwdat2
   Locate 3 , 4
   For I = 1 To 16
      Read Dat_s
      Decival = Binval(dat_s)
      Lcd Chr(decival)
   Next
   Waitms 3000
   Cls

   Locate 2 , 1 : Lcd "Temp. Sesors:"
   Waitms 500
   Nr = 1wirecount()
   If Nr <> 0 Then
      Locate 3 , 1 : Lcd Nr ; " sensors available"
      'find the first device on 1Wire (Temp Sensors)
      Dsid(1) = 1wsearchfirst()
      'search for other devices
      Do
         Dsid(1) = 1wsearchnext()
      Loop Until Err = 1
   Else
      Locate 3 , 1 : Lcd "no sensors found !"
   End If

   Waitms 1500
   Cls
End Sub

Sub Ds_temp(byval Sensor_nr As Byte)
Local Snr As Byte
   ' Measure all the other DS1820
   1wreset
   1wwrite &HCC
   1wwrite &H44
   Waitms 750
   'Chose the desired sensor
   1wverify Dsid(sensor_nr)
   'READ SCRATCHPAD
   1wwrite &HBE
   Sc(1) = 1wread(9)
   If Sc(9) = Crc8(sc(1) , 8) Then
      Tmp = Sc(1) And 1                                     ' 0.1C precision
      If Tmp = 1 Then Decr Sc(1)

      T = Makeint(sc(1) , Sc(2))

      T = T * 50 : T = T - 25 : T1 = Sc(8) - Sc(7) : T1 = T1 * 100
      T1 = T1 / Sc(8) : T = T + T1 : T = T / 100
      Temperature = Str(t)
      Tempdecimal = Right(temperature , 1)
      U = Len(temperature) - 1
      Tempunit = Left(temperature , U)
      'show the temperature in °C
   End If
End Sub

Sub Send_list
   Set Led
   For I = 1 To 9
      J = I * 16
      Address = &H000 + J
      Readeeprom Station(i) , Address
   Next

   For I = 1 To 9
      J = I * 16
      Address = &H090 + J
      Readeeprom F(i) , Address
   Next

   Print "# " ;
   For I = 1 To 9
      Print Station(i) ; "@" ; F(i) ; "/";
      Waitms 400
   Next

   Print "!"
   Waitms 100
   Print "!list sent"

   Waitms 500
   Reset Led
End Sub

Sub Load_radio_stations
   Readeeprom Station(1) , &H010
   If Asc(station(1) , 1) = 255 Then Station(1) = ""
   Readeeprom F(1) , &H0A0
   If F(1) <> "" Then
      Radio_freq(1) = Val(f(1))
   Else
      Radio_freq(1) = 0
   End If

   Readeeprom Station(2) , &H020
   If Asc(station(2) , 1) = 255 Then Station(2) = ""
   Readeeprom F(2) , &H0B0
   If F(2) <> "" Then
      Radio_freq(2) = Val(f(2))
   Else
      Radio_freq(2) = 0
   End If

   Readeeprom Station(3) , &H030
   If Asc(station(3) , 1) = 255 Then Station(3) = ""
   Readeeprom F(3) , &H0C0
   If F(3) <> "" Then
      Radio_freq(3) = Val(f(3))
   Else
      Radio_freq(3) = 0
   End If

   Readeeprom Station(4) , &H040
   If Asc(station(4) , 1) = 255 Then Station(4) = ""
   Readeeprom F(4) , &H0D0
   If F(4) <> "" Then
      Radio_freq(4) = Val(f(4))
   Else
      Radio_freq(4) = 0
   End If

   Readeeprom Station(5) , &H050
   If Asc(station(5) , 1) = 255 Then Station(5) = ""
   Readeeprom F(5) , &H0E0
   If F(5) <> "" Then
      Radio_freq(5) = Val(f(5))
   Else
      Radio_freq(5) = 0
   End If

   Readeeprom Station(6) , &H060
   If Asc(station(6) , 1) = 255 Then Station(6) = ""
   Readeeprom F(6) , &H0F0
   If F(6) <> "" Then
      Radio_freq(6) = Val(f(6))
   Else
      Radio_freq(6) = 0
   End If

   Readeeprom Station(7) , &H070
   If Asc(station(7) , 1) = 255 Then Station(7) = ""
   Readeeprom F(7) , &H100
   If F(7) <> "" Then
      Radio_freq(7) = Val(f(7))
   Else
      Radio_freq(7) = 0
   End If

   Readeeprom Station(8) , &H080
   If Asc(station(8) , 1) = 255 Then Station(8) = ""
   Readeeprom F(8) , &H110
   If F(8) <> "" Then
      Radio_freq(8) = Val(f(8))
   Else
      Radio_freq(8) = 0
   End If

   Readeeprom Station(9) , &H090
   If Asc(station(9) , 1) = 255 Then Station(9) = ""
   Readeeprom F(9) , &H120
   If F(9) <> "" Then
      Radio_freq(9) = Val(f(9))
   Else
      Radio_freq(9) = 0
   End If

End Sub

'For DEBUG
Sub Test_stations

   For Station_pos = 1 To 9
      J = Station_pos * 16
      Ad_s = &H000 + J
      Ad_f = &H090 + J

      Station(station_pos) = Chr(0)
      F(station_pos) = Chr(0)
      For I = 0 To 15
         Address = Ad_s + I
         Writeeeprom Station(station_pos) , Address
         Address = Ad_f + I
         Writeeeprom F(station_pos) , Address
      Next
   Next

   'test values
   Radio_freq(1) = 87.6
   Station(1) = "Eins Live"

   Radio_freq(2) = 87.94
   Station(2) = "WDR 5"

   Radio_freq(3) = 98.6
   Station(3) = "WDR 2"

   Radio_freq(4) = 88.7
   Station(4) = "Deutschland RB"

   Radio_freq(5) = 89.10
   Station(5) = "Deutschlandfunk"

   Radio_freq(6) = 95.1
   Station(6) = "WDR 3"

   Radio_freq(7) = 90.70
   Station(7) = "WDR 4"

   Radio_freq(8) = 99.70
   Station(8) = "N JOY"

   Radio_freq(9) = 94.8
   Station(9) = "SWR 3"

   Writeeeprom Station(1) , &H010
   F(1) = Fusing(radio_freq(1) , "#.###")
   Writeeeprom F(1) , &H0A0

   Writeeeprom Station(2) , &H020
   F(2) = Fusing(radio_freq(2) , "#.###")
   Writeeeprom F(2) , &H0B0

   Writeeeprom Station(3) , &H030
   F(3) = Fusing(radio_freq(3) , "#.###")
   Writeeeprom F(3) , &H0C0

   Writeeeprom Station(4) , &H040
   F(4) = Fusing(radio_freq(4) , "#.###")
   Writeeeprom F(4) , &H0D0

   Writeeeprom Station(5) , &H050
   F(5) = Fusing(radio_freq(5) , "#.###")
   Writeeeprom F(5) , &H0E0

   Writeeeprom Station(6) , &H060
   F(6) = Fusing(radio_freq(6) , "#.###")
   Writeeeprom F(6) , &H0F0

   Writeeeprom Station(7) , &H070
   F(7) = Fusing(radio_freq(7) , "#.###")
   Writeeeprom F(7) , &H100

   Writeeeprom Station(8) , &H080
   F(8) = Fusing(radio_freq(8) , "#.###")
   Writeeeprom F(8) , &H110

   Writeeeprom Station(9) , &H090
   F(9) = Fusing(radio_freq(9) , "#.###")
   Writeeeprom F(9) , &H120

End Sub

Weekdays:
Data "Monday" , "Tuesday" , "Wednesday" , "Thursday" , "Friday" , "Saturday" , "Sunday"

Fwdat1:
Data "01010010" , "01100001" , "01100100" , "01101001" , "01101111" , "00100000"
Data "01000001" , "01101100" , "01100001" , "01110010" , "01101101" , "00100000"
Data "01000011" , "01101100" , "01101111" , "01100011" , "01101011"

Fwdat2:
Data "01100011" , "01101111" , "01110010" , "01100101" , "01010111" , "01100101"
Data "01100001" , "01110110" , "01100101" , "01110010" , "00100000" , "00100000"
Data "00110010" , "00110000" , "00110010" , "00110010"