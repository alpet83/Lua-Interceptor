unit ByteCode;

interface
uses Windows, SysUtils;
{$D-}
{$I stkdef.inc}
{$IFDEF NLC}
 {$IFDEF NEWEST_BUILD}
  {$I scripts\savemgmt_new.inc}
 {$ELSE}
  {$I scripts\savemgmt.inc}
 {$ENDIF}
  {$I scripts\profiles.inc}
  {$I scripts\hidden.inc}
{$ENDIF}

implementation

end.

// Updated 21.11.2014 13:49:18.25 
// Updated 21.11.2014 16:38:31.46 
// Updated 21.11.2014 16:53:52.43 
// Updated 22.11.2014 15:15:29.91 
// Updated 23.11.2014 21:39:17.36 
// Updated 27.11.2014 18:23:17.76 
// Updated 27.11.2014 18:59:55.57 
// Updated 27.11.2014 19:34:41.51 
// Updated 30.11.2014  0:45:47.00 
// Updated 30.11.2014 12:18:21.40 
// Updated 03.12.2014 17:08:09.56 
// Updated 26.12.2014 14:48:27.57 
// Updated 28.12.2014 20:30:01.08 
// Updated 29.12.2014 10:51:16.28 
// Updated 20.01.2015  0:13:22.54 
// Updated 23.01.2015 12:02:01.72 
// Updated 13.05.2015 17:01:19.93 
// Updated 13.05.2015 17:10:44.18 
// Updated 27.06.2015 14:37:12.52 
// Updated 03.09.2015 20:58:27.42 
// Updated 03.09.2015 21:07:36.46 
// Updated 03.09.2015 22:51:23.18 
// Updated 18.11.2015 11:54:51.21 
// Updated 21.12.2015 12:11:45.60 
// Updated 25.12.2015 11:53:21.87 
// Updated 25.12.2015 11:53:50.69 
// Updated 25.12.2015 12:07:23.02 
// Updated 25.12.2015 13:09:41.03 
// Updated 26.12.2015 16:53:53.23 
// Updated 27.12.2015 21:43:00.04 
// Updated 27.12.2015 22:26:07.79 
// Updated 28.12.2015 21:02:21.30 
// Updated 30.12.2015  2:01:30.85 
// Updated 30.12.2015  2:14:54.61 
// Updated 30.12.2015  2:24:38.50 
// Updated 01.01.2016 14:40:49.23 
// Updated 05.01.2016 18:29:02.54 
// Updated 05.01.2016 22:54:06.53 
// Updated 05.01.2016 23:23:55.55 
// Updated 05.01.2016 23:32:49.90 
// Updated 05.01.2016 23:45:56.61 
// Updated 06.01.2016  0:00:41.04 
// Updated 06.01.2016  0:25:04.27 
// Updated 06.01.2016 11:52:23.67 
// Updated 06.01.2016 12:07:31.16 
// Updated 06.01.2016 12:15:05.73 
// Updated 06.01.2016 12:24:51.42 
// Updated 06.01.2016 12:32:13.93 
// Updated 07.01.2016 14:56:32.48 
// Updated 10.01.2016 13:05:28.73 
// Updated 11.01.2016  0:20:24.20 
// Updated 11.01.2016  1:09:00.50 
// Updated 11.01.2016  1:19:49.87 
// Updated 15.01.2016 20:07:53.60 
// Updated 18.01.2016 11:26:48.93 
// Updated 28.01.2016 14:13:25.96 
// Updated 28.01.2016 16:32:10.67 
// Updated 29.01.2016 14:59:50.84 
// Updated 29.01.2016 19:08:43.01 
// Updated 11.02.2016 16:06:40.01 
// Updated 04.03.2016 10:44:33.03 
// Updated 16.03.2016 11:06:21.68 
// Updated 21.03.2016 11:33:15.31 
// Updated 19.05.2016 22:24:37.91 
// Updated 22.05.2016 22:46:40.17 
