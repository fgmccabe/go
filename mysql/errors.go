mysql.errors{

  /*
   * A listing of the standard mysql error codes
   */

  mysql_error_code:[string,integer]{}.

  mysql_error_code("ERROR FIRST",1000).
  mysql_error_code("HASHCHK",1000).
  mysql_error_code("NISAMCHK",1001).
  mysql_error_code("NO",1002).
  mysql_error_code("YES",1003).
  mysql_error_code("CANT CREATE FILE",1004).
  mysql_error_code("CANT CREATE TABLE",1005).
  mysql_error_code("CANT CREATE DB",1006).
  mysql_error_code("DB CREATE EXISTS",1007).
  mysql_error_code("DB DROP EXISTS",1008).
  mysql_error_code("DB DROP DELETE",1009).
  mysql_error_code("DB DROP RMDIR",1010).
  mysql_error_code("CANT DELETE FILE",1011).
  mysql_error_code("CANT FIND SYSTEM REC",1012).
  mysql_error_code("CANT GET STAT",1013).
  mysql_error_code("CANT GET WD",1014).
  mysql_error_code("CANT LOCK",1015).
  mysql_error_code("CANT OPEN FILE",1016).
  mysql_error_code("FILE NOT FOUND",1017).
  mysql_error_code("CANT READ DIR",1018).
  mysql_error_code("CANT SET WD",1019).
  mysql_error_code("CHECKREAD",1020).
  mysql_error_code("DISK FULL",1021).
  mysql_error_code("DUP KEY",1022).
  mysql_error_code("ERROR ON CLOSE",1023).
  mysql_error_code("ERROR ON READ",1024).
  mysql_error_code("ERROR ON RENAME",1025).
  mysql_error_code("ERROR ON WRITE",1026).
  mysql_error_code("FILE USED",1027).
  mysql_error_code("FILSORT ABORT",1028).
  mysql_error_code("FORM NOT FOUND",1029).
  mysql_error_code("GET ERRNO",1030).
  mysql_error_code("ILLEGAL HA",1031).
  mysql_error_code("KEY NOT FOUND",1032).
  mysql_error_code("NOT FORM FILE",1033).
  mysql_error_code("NOT KEYFILE",1034).
  mysql_error_code("OLD KEYFILE",1035).
  mysql_error_code("OPEN AS READONLY",1036).
  mysql_error_code("OUTOFMEMORY",1037).
  mysql_error_code("OUT OF SORTMEMORY",1038).
  mysql_error_code("UNEXPECTED EOF",1039).
  mysql_error_code("CON COUNT ERROR",1040).
  mysql_error_code("OUT OF RESOURCES",1041).
  mysql_error_code("BAD HOST ERROR",1042).
  mysql_error_code("HANDSHAKE ERROR",1043).
  mysql_error_code("DBACCESS DENIED ERROR",1044).
  mysql_error_code("ACCESS DENIED ERROR",1045).
  mysql_error_code("NO DB ERROR",1046).
  mysql_error_code("UNKNOWN COM ERROR",1047).
  mysql_error_code("BAD NULL ERROR",1048).
  mysql_error_code("BAD DB ERROR",1049).
  mysql_error_code("TABLE EXISTS ERROR",1050).
  mysql_error_code("BAD TABLE ERROR",1051).
  mysql_error_code("NON UNIQ ERROR",1052).
  mysql_error_code("SERVSHUTDOWN",1053).
  mysql_error_code("BAD FIELD ERROR",1054).
  mysql_error_code("WRONG FIELD WITH GROUP",1055).
  mysql_error_code("WRONG GROUP FIELD",1056).
  mysql_error_code("WRONG SUM SELECT",1057).
  mysql_error_code("WRONG VALUE COUNT",1058).
  mysql_error_code("TOO LONG IDENT",1059).
  mysql_error_code("DUP FIELDNAME",1060).
  mysql_error_code("DUP KEYNAME",1061).
  mysql_error_code("DUP ENTRY",1062).
  mysql_error_code("WRONG FIELD SPEC",1063).
  mysql_error_code("PARSE ERROR",1064).
  mysql_error_code("EMPTY QUERY",1065).
  mysql_error_code("NONUNIQ TABLE",1066).
  mysql_error_code("INVALID DEFAULT",1067).
  mysql_error_code("MULTIPLE PRI KEY",1068).
  mysql_error_code("TOO MANY KEYS",1069).
  mysql_error_code("TOO MANY KEY PARTS",1070).
  mysql_error_code("TOO LONG KEY",1071).
  mysql_error_code("KEY COLUMN DOES NOT EXITS",1072).
  mysql_error_code("BLOB USED AS KEY",1073).
  mysql_error_code("TOO BIG FIELDLENGTH",1074).
  mysql_error_code("WRONG AUTO KEY",1075).
  mysql_error_code("READY",1076).
  mysql_error_code("NORMAL SHUTDOWN",1077).
  mysql_error_code("GOT SIGNAL",1078).
  mysql_error_code("SHUTDOWN COMPLETE",1079).
  mysql_error_code("FORCING CLOSE",1080).
  mysql_error_code("IPSOCK ERROR",1081).
  mysql_error_code("NO SUCH INDEX",1082).
  mysql_error_code("WRONG FIELD TERMINATORS",1083).
  mysql_error_code("BLOBS AND NO TERMINATED",1084).
  mysql_error_code("TEXTFILE NOT READABLE",1085).
  mysql_error_code("FILE EXISTS ERROR",1086).
  mysql_error_code("LOAD INFO",1087).
  mysql_error_code("ALTINFO",1088).
  mysql_error_code("WRONG SUB KEY",1089).
  mysql_error_code("CANT REMOVE ALL FIELDS",1090).
  mysql_error_code("CANT DROP FIELD OR KEY",1091).
  mysql_error_code("INSERT INFO",1092).
  mysql_error_code("UPDATE TABLE USED",1093).
  mysql_error_code("NO SUCH THREAD",1094).
  mysql_error_code("KILL DENIED ERROR",1095).
  mysql_error_code("NO TABLES USED",1096).
  mysql_error_code("TOO BIG SET",1097).
  mysql_error_code("NO UNIQUE LOGFILE",1098).
  mysql_error_code("TABLE NOT LOCKED FOR WRITE",1099).
  mysql_error_code("TABLE NOT LOCKED",1100).
  mysql_error_code("BLOB CANT HAVE DEFAULT",1101).
  mysql_error_code("WRONG DB NAME",1102).
  mysql_error_code("WRONG TABLE NAME",1103).
  mysql_error_code("TOO BIG SELECT",1104).
  mysql_error_code("UNKNOWN ERROR",1105).
  mysql_error_code("UNKNOWN PROCEDURE",1106).
  mysql_error_code("WRONG PARAMCOUNT TO PROCEDURE",1107).
  mysql_error_code("WRONG PARAMETERS TO PROCEDURE",1108).
  mysql_error_code("UNKNOWN TABLE",1109).
  mysql_error_code("FIELD SPECIFIED TWICE",1110).
  mysql_error_code("INVALID GROUP FUNC USE",1111).
  mysql_error_code("UNSUPPORTED EXTENSION",1112).
  mysql_error_code("TABLE MUST HAVE COLUMNS",1113).
  mysql_error_code("RECORD FILE FULL",1114).
  mysql_error_code("UNKNOWN CHARACTSET",1115).
  mysql_error_code("TOO MANY TABLES",1116).
  mysql_error_code("TOO MANY FIELDS",1117).
  mysql_error_code("TOO BIG ROWSIZE",1118).
  mysql_error_code("STACK OVERRUN",1119).
  mysql_error_code("WRONG OUTJOIN",1120).
  mysql_error_code("NULL COLUMN IN INDEX",1121).
  mysql_error_code("CANT FIND UDF",1122).
  mysql_error_code("CANT INITIALIZE UDF",1123).
  mysql_error_code("UDF NO PATHS",1124).
  mysql_error_code("UDF EXISTS",1125).
  mysql_error_code("CANT OPEN LIBRARY",1126).
  mysql_error_code("CANT FIND DL ENTRY",1127).
  mysql_error_code("FUNCTION NOT DEFINED",1128).
  mysql_error_code("HOST IS BLOCKED",1129).
  mysql_error_code("HOST NOT PRIVILEGED",1130).
  mysql_error_code("PASSWORD ANONYMOUS USER",1131).
  mysql_error_code("PASSWORD NOT ALLOWED",1132).
  mysql_error_code("PASSWORD NO MATCH",1133).
  mysql_error_code("UPDATE INFO",1134).
  mysql_error_code("CANT CREATE THREAD",1135).
  mysql_error_code("WRONG VALUE COUNT ON ROW",1136).
  mysql_error_code("CANT REOPEN TABLE",1137).
  mysql_error_code("INVALID USE OF NULL",1138).
  mysql_error_code("REGEXP ERROR",1139).
  mysql_error_code("MIX OF GROUP FUNC AND FIELDS",1140).
  mysql_error_code("NONEXISTING GRANT",1141).
  mysql_error_code("TABLEACCESS DENIED ERROR",1142).
  mysql_error_code("COLUMNACCESS DENIED ERROR",1143).
  mysql_error_code("ILLEGAL GRANT FOR TABLE",1144).
  mysql_error_code("GRANT WRONG HOST OR USER",1145).
  mysql_error_code("NO SUCH TABLE",1146).
  mysql_error_code("NONEXISTING TABLE GRANT",1147).
  mysql_error_code("NOT ALLOWED COMMAND",1148).
  mysql_error_code("SYNTAX ERROR",1149).
  mysql_error_code("DELAYED CANT CHANGE LOCK",1150).
  mysql_error_code("TOO MANY DELAYED THREADS",1151).
  mysql_error_code("ABORTING CONNECTION",1152).
  mysql_error_code("NET PACKET TOO LARGE",1153).
  mysql_error_code("NET READ ERROR FROM PIPE",1154).
  mysql_error_code("NET FCNTL ERROR",1155).
  mysql_error_code("NET PACKETS OUT OF ORDER",1156).
  mysql_error_code("NET UNCOMPRESS ERROR",1157).
  mysql_error_code("NET READ ERROR",1158).
  mysql_error_code("NET READ INTERRUPTED",1159).
  mysql_error_code("NET ERROR ON WRITE",1160).
  mysql_error_code("NET WRITE INTERRUPTED",1161).
  mysql_error_code("TOO LONG STRING",1162).
  mysql_error_code("TABLE CANT HANDLE BLOB",1163).
  mysql_error_code("TABLE CANT HANDLE AUTO INCREMENT",1164).
  mysql_error_code("DELAYED INSERT TABLE LOCKED",1165).
  mysql_error_code("WRONG COLUMN NAME",1166).
  mysql_error_code("WRONG KEY COLUMN",1167).
  mysql_error_code("WRONG MRG TABLE",1168).
  mysql_error_code("DUP UNIQUE",1169).
  mysql_error_code("BLOB KEY WITHOUT LENGTH",1170).
  mysql_error_code("PRIMARY CANT HAVE NULL",1171).
  mysql_error_code("TOO MANY ROWS",1172).
  mysql_error_code("REQUIRES PRIMARY KEY",1173).
  mysql_error_code("NO RAID COMPILED",1174).
  mysql_error_code("UPDATE WITHOUT KEY IN SAFE MODE",1175).
  mysql_error_code("KEY DOES NOT EXITS",1176).
  mysql_error_code("CHECK NO SUCH TABLE",1177).
  mysql_error_code("CHECK NOT IMPLEMENTED",1178).
  mysql_error_code("CANT DO THIS DURING AN TRANSACTION",1179).
  mysql_error_code("ERROR DURING COMMIT",1180).
  mysql_error_code("ERROR DURING ROLLBACK",1181).
  mysql_error_code("ERROR DURING FLUSH LOGS",1182).
  mysql_error_code("ERROR DURING CHECKPOINT",1183).
  mysql_error_code("NEW ABORTING CONNECTION",1184).
  mysql_error_code("DUMP NOT IMPLEMENTED",1185).
  mysql_error_code("FLUSH MASTBINLOG CLOSED",1186).
  mysql_error_code("INDEX REBUILD",1187).
  mysql_error_code("MASTER",1188).
  mysql_error_code("MASTNET READ",1189).
  mysql_error_code("MASTNET WRITE",1190).
  mysql_error_code("FT MATCHING KEY NOT FOUND",1191).
  mysql_error_code("LOCK OR ACTIVE TRANSACTION",1192).
  mysql_error_code("UNKNOWN SYSTEM VARIABLE",1193).
  mysql_error_code("CRASHED ON USAGE",1194).
  mysql_error_code("CRASHED ON REPAIR",1195).
  mysql_error_code("WARNING NOT COMPLETE ROLLBACK",1196).
  mysql_error_code("TRANS CACHE FULL",1197).
  mysql_error_code("SLAVE MUST STOP",1198).
  mysql_error_code("SLAVE NOT RUNNING",1199).
  mysql_error_code("BAD SLAVE",1200).
  mysql_error_code("MASTINFO",1201).
  mysql_error_code("SLAVE THREAD",1202).
  mysql_error_code("TOO MANY USCONNECTIONS",1203).
  mysql_error_code("SET CONSTANTS ONLY",1204).
  mysql_error_code("LOCK WAIT TIMEOUT",1205).
  mysql_error_code("LOCK TABLE FULL",1206).
  mysql_error_code("READ ONLY TRANSACTION",1207).
  mysql_error_code("DROP DB WITH READ LOCK",1208).
  mysql_error_code("CREATE DB WITH READ LOCK",1209).
  mysql_error_code("WRONG ARGUMENTS",1210).
  mysql_error_code("NO PERMISSION TO CREATE USER",1211).
  mysql_error_code("UNION TABLES IN DIFFERENT DIR",1212).
  mysql_error_code("LOCK DEADLOCK",1213).
  mysql_error_code("TABLE CANT HANDLE FT",1214).
  mysql_error_code("CANNOT ADD FOREIGN",1215).
  mysql_error_code("NO REFERENCED ROW",1216).
  mysql_error_code("ROW IS REFERENCED",1217).
  mysql_error_code("CONNECT TO MASTER",1218).
  mysql_error_code("QUERY ON MASTER",1219).
  mysql_error_code("ERROR WHEN EXECUTING COMMAND",1220).
  mysql_error_code("WRONG USAGE",1221).
  mysql_error_code("WRONG NUMBOF COLUMNS IN SELECT",1222).
  mysql_error_code("CANT UPDATE WITH READLOCK",1223).
  mysql_error_code("MIXING NOT ALLOWED",1224).
  mysql_error_code("DUP ARGUMENT",1225).
  mysql_error_code("USLIMIT REACHED",1226).
  mysql_error_code("SPECIFIC ACCESS DENIED ERROR",1227).
  mysql_error_code("LOCAL VARIABLE",1228).
  mysql_error_code("GLOBAL VARIABLE",1229).
  mysql_error_code("NO DEFAULT",1230).
  mysql_error_code("WRONG VALUE FOR VAR",1231).
  mysql_error_code("WRONG TYPE FOR VAR",1232).
  mysql_error_code("VAR CANT BE READ",1233).
  mysql_error_code("CANT USE OPTION HERE",1234).
  mysql_error_code("NOT SUPPORTED YET",1235).
  mysql_error_code("MASTFATAL ERROR READING BINLOG",1236).
  mysql_error_code("SLAVE IGNORED TABLE",1237).
  mysql_error_code("INCORRECT GLOBAL LOCAL VAR",1238).
  mysql_error_code("WRONG FK DEF",1239).
  mysql_error_code("KEY REF DO NOT MATCH TABLE REF",1240).
  mysql_error_code("OPERAND COLUMNS",1241).
  mysql_error_code("SUBQUERY NO 1 ROW",1242).
  mysql_error_code("UNKNOWN STMT HANDLER",1243).
  mysql_error_code("CORRUPT HELP DB",1244).
  mysql_error_code("CYCLIC REFERENCE",1245).
  mysql_error_code("AUTO CONVERT",1246).
  mysql_error_code("ILLEGAL REFERENCE",1247).
  mysql_error_code("DERIVED MUST HAVE ALIAS",1248).
  mysql_error_code("SELECT REDUCED",1249).
  mysql_error_code("TABLENAME NOT ALLOWED HERE",1250).
  mysql_error_code("NOT SUPPORTED AUTH MODE",1251).
  mysql_error_code("SPATIAL CANT HAVE NULL",1252).
  mysql_error_code("COLLATION CHARSET MISMATCH",1253).
  mysql_error_code("SLAVE WAS RUNNING",1254).
  mysql_error_code("SLAVE WAS NOT RUNNING",1255).
  mysql_error_code("TOO BIG FOR UNCOMPRESS",1256).
  mysql_error_code("ZLIB Z MEM ERROR",1257).
  mysql_error_code("ZLIB Z BUF ERROR",1258).
  mysql_error_code("ZLIB Z DATA ERROR",1259).
  mysql_error_code("CUT VALUE GROUP CONCAT",1260).
  mysql_error_code("WARN TOO FEW RECORDS",1261).
  mysql_error_code("WARN TOO MANY RECORDS",1262).
  mysql_error_code("WARN NULL TO NOTNULL",1263).
  mysql_error_code("WARN DATA OUT OF RANGE",1264).
  mysql_error_code("WARN DATA TRUNCATED",1265).
  mysql_error_code("WARN USING OTHHANDLER",1266).
  mysql_error_code("CANT AGGREGATE 2 COLLATIONS",1267).
  mysql_error_code("DROP USER",1268).
  mysql_error_code("REVOKE GRANTS",1269).
  mysql_error_code("CANT AGGREGATE 3 COLLATIONS",1270).
  mysql_error_code("CANT AGGREGATE NCOLLATIONS",1271).
  mysql_error_code("VARIABLE IS NOT STRUCT",1272).
  mysql_error_code("UNKNOWN COLLATION",1273).
  mysql_error_code("SLAVE IGNORED SSL PARAMS",1274).
  mysql_error_code("SERVIS IN SECURE AUTH MODE",1275).
  mysql_error_code("WARN FIELD RESOLVED",1276).
  mysql_error_code("BAD SLAVE UNTIL COND",1277).
  mysql_error_code("MISSING SKIP SLAVE",1278).
  mysql_error_code("UNTIL COND IGNORED",1279).
  mysql_error_code("WRONG NAME FOR INDEX",1280).
  mysql_error_code("WRONG NAME FOR CATALOG",1281).
  mysql_error_code("WARN QC RESIZE",1282).
  mysql_error_code("BAD FT COLUMN",1283).
  mysql_error_code("UNKNOWN KEY CACHE",1284).
  mysql_error_code("WARN HOSTNAME WONT WORK",1285).
  mysql_error_code("UNKNOWN STORAGE ENGINE",1286).
  mysql_error_code("WARN DEPRECATED SYNTAX",1287).
  mysql_error_code("NON UPDATABLE TABLE",1288).
  mysql_error_code("FEATURE DISABLED",1289).
  mysql_error_code("OPTION PREVENTS STATEMENT",1290).
  mysql_error_code("DUPLICATED VALUE IN TYPE",1291).
  mysql_error_code("TRUNCATED WRONG VALUE",1292).
  mysql_error_code("TOO MUCH AUTO TIMESTAMP COLS",1293).
  mysql_error_code("INVALID ON UPDATE",1294).
  mysql_error_code("UNSUPPORTED PS",1295).
  mysql_error_code("GET ERRMSG",1296).
  mysql_error_code("GET TEMPORARY ERRMSG",1297).
  mysql_error_code("UNKNOWN TIME ZONE",1298).
  mysql_error_code("WARN INVALID TIMESTAMP",1299).
  mysql_error_code("INVALID CHARACTSTRING",1300).
  mysql_error_code("WARN ALLOWED PACKET OVERFLOWED",1301).
  mysql_error_code("CONFLICTING DECLARATIONS",1302).
  mysql_error_code("SP NO RECURSIVE CREATE",1303).
  mysql_error_code("SP ALREADY EXISTS",1304).
  mysql_error_code("SP DOES NOT EXIST",1305).
  mysql_error_code("SP DROP FAILED",1306).
  mysql_error_code("SP STORE FAILED",1307).
  mysql_error_code("SP LILABEL MISMATCH",1308).
  mysql_error_code("SP LABEL REDEFINE",1309).
  mysql_error_code("SP LABEL MISMATCH",1310).
  mysql_error_code("SP UNINIT VAR",1311).
  mysql_error_code("SP BADSELECT",1312).
  mysql_error_code("SP BADRETURN",1313).
  mysql_error_code("SP BADSTATEMENT",1314).
  mysql_error_code("UPDATE LOG DEPRECATED IGNORED",1315).
  mysql_error_code("UPDATE LOG DEPRECATED TRANSLATED",1316).
  mysql_error_code("QUERY INTERRUPTED",1317).
  mysql_error_code("SP WRONG NO OF ARGS",1318).
  mysql_error_code("SP COND MISMATCH",1319).
  mysql_error_code("SP NORETURN",1320).
  mysql_error_code("SP NORETURNEND",1321).
  mysql_error_code("SP BAD CURSOR QUERY",1322).
  mysql_error_code("SP BAD CURSOR SELECT",1323).
  mysql_error_code("SP CURSOR MISMATCH",1324).
  mysql_error_code("SP CURSOR ALREADY OPEN",1325).
  mysql_error_code("SP CURSOR NOT OPEN",1326).
  mysql_error_code("SP UNDECLARED VAR",1327).
  mysql_error_code("SP WRONG NO OF FETCH ARGS",1328).
  mysql_error_code("SP FETCH NO DATA",1329).
  mysql_error_code("SP DUP PARAM",1330).
  mysql_error_code("SP DUP VAR",1331).
  mysql_error_code("SP DUP COND",1332).
  mysql_error_code("SP DUP CURS",1333).
  mysql_error_code("SP CANT ALTER",1334).
  mysql_error_code("SP SUBSELECT NYI",1335).
  mysql_error_code("STMT NOT ALLOWED IN SF OR TRG",1336).
  mysql_error_code("SP VARCOND AFTCURSHNDLR",1337).
  mysql_error_code("SP CURSOR AFTHANDLER",1338).
  mysql_error_code("SP CASE NOT FOUND",1339).
  mysql_error_code("FPARSTOO BIG FILE",1340).
  mysql_error_code("FPARSBAD HEADER",1341).
  mysql_error_code("FPARSEOF IN COMMENT",1342).
  mysql_error_code("FPARSERROR IN PARAMETER",1343).
  mysql_error_code("FPARSEOF IN UNKNOWN PARAMETER",1344).
  mysql_error_code("VIEW NO EXPLAIN",1345).
  mysql_error_code("FRM UNKNOWN TYPE",1346).
  mysql_error_code("WRONG OBJECT",1347).
  mysql_error_code("NONUPDATEABLE COLUMN",1348).
  mysql_error_code("VIEW SELECT DERIVED",1349).
  mysql_error_code("VIEW SELECT CLAUSE",1350).
  mysql_error_code("VIEW SELECT VARIABLE",1351).
  mysql_error_code("VIEW SELECT TMPTABLE",1352).
  mysql_error_code("VIEW WRONG LIST",1353).
  mysql_error_code("WARN VIEW MERGE",1354).
  mysql_error_code("WARN VIEW WITHOUT KEY",1355).
  mysql_error_code("VIEW INVALID",1356).
  mysql_error_code("SP NO DROP SP",1357).
  mysql_error_code("SP GOTO IN HNDLR",1358).
  mysql_error_code("TRG ALREADY EXISTS",1359).
  mysql_error_code("TRG DOES NOT EXIST",1360).
  mysql_error_code("TRG ON VIEW OR TEMP TABLE",1361).
  mysql_error_code("TRG CANT CHANGE ROW",1362).
  mysql_error_code("TRG NO SUCH ROW IN TRG",1363).
  mysql_error_code("NO DEFAULT FOR FIELD",1364).
  mysql_error_code("DIVISION BY ZERO",1365).
  mysql_error_code("TRUNCATED WRONG VALUE FOR FIELD",1366).
  mysql_error_code("ILLEGAL VALUE FOR TYPE",1367).
  mysql_error_code("VIEW NONUPD CHECK",1368).
  mysql_error_code("VIEW CHECK FAILED",1369).
  mysql_error_code("PROCACCESS DENIED ERROR",1370).
  mysql_error_code("RELAY LOG FAIL",1371).
  mysql_error_code("PASSWD LENGTH",1372).
  mysql_error_code("UNKNOWN TARGET BINLOG",1373).
  mysql_error_code("IO ERR LOG INDEX READ",1374).
  mysql_error_code("BINLOG PURGE PROHIBITED",1375).
  mysql_error_code("FSEEK FAIL",1376).
  mysql_error_code("BINLOG PURGE FATAL ERR",1377).
  mysql_error_code("LOG IN USE",1378).
  mysql_error_code("LOG PURGE UNKNOWN ERR",1379).
  mysql_error_code("RELAY LOG INIT",1380).
  mysql_error_code("NO BINARY LOGGING",1381).
  mysql_error_code("RESERVED SYNTAX",1382).
  mysql_error_code("WSAS FAILED",1383).
  mysql_error_code("DIFF GROUPS PROC",1384).
  mysql_error_code("NO GROUP FOR PROC",1385).
  mysql_error_code("ORDWITH PROC",1386).
  mysql_error_code("LOGGING PROHIBIT CHANGING OF",1387).
  mysql_error_code("NO FILE MAPPING",1388).
  mysql_error_code("WRONG MAGIC",1389).
  mysql_error_code("PS MANY PARAM",1390).
  mysql_error_code("KEY PART 0",1391).
  mysql_error_code("VIEW CHECKSUM",1392).
  mysql_error_code("VIEW MULTIUPDATE",1393).
  mysql_error_code("VIEW NO INSERT FIELD LIST",1394).
  mysql_error_code("VIEW DELETE MERGE VIEW",1395).
  mysql_error_code("CANNOT USER",1396).
  mysql_error_code("XANOTA",1397).
  mysql_error_code("XAINVAL",1398).
  mysql_error_code("XARMFAIL",1399).
  mysql_error_code("XAOUTSIDE",1400).
  mysql_error_code("XARMERR",1401).
  mysql_error_code("XA RBROLLBACK",1402).
  mysql_error_code("NONEXISTING PROC GRANT",1403).
  mysql_error_code("PROC AUTO GRANT FAIL",1404).
  mysql_error_code("PROC AUTO REVOKE FAIL",1405).
  mysql_error_code("DATA TOO LONG",1406).
  mysql_error_code("SP BAD SQLSTATE",1407).
  mysql_error_code("STARTUP",1408).
  mysql_error_code("LOAD FROM FIXED SIZE ROWS TO VAR",1409).
  mysql_error_code("CANT CREATE USWITH GRANT",1410).
  mysql_error_code("WRONG VALUE FOR TYPE",1411).
  mysql_error_code("TABLE DEF CHANGED",1412).
  mysql_error_code("SP DUP HANDLER",1413).
  mysql_error_code("SP NOT VAR ARG",1414).
  mysql_error_code("SP NO RETSET",1415).
  mysql_error_code("CANT CREATE GEOMETRY OBJECT",1416).
  mysql_error_code("FAILED ROUTINE BREAK BINLOG",1417).
  mysql_error_code("BINLOG UNSAFE ROUTINE",1418).
  mysql_error_code("BINLOG CREATE ROUTINE NEED SUPER",1419).
  mysql_error_code("EXEC STMT WITH OPEN CURSOR",1420).
  mysql_error_code("STMT HAS NO OPEN CURSOR",1421).
  mysql_error_code("COMMIT NOT ALLOWED IN SF OR TRG",1422).
  mysql_error_code("NO DEFAULT FOR VIEW FIELD",1423).
  mysql_error_code("SP NO RECURSION",1424).
  mysql_error_code("TOO BIG SCALE",1425).
  mysql_error_code("TOO BIG PRECISION",1426).
  mysql_error_code("M BIGGTHAN D",1427).
  mysql_error_code("WRONG LOCK OF SYSTEM TABLE",1428).
  mysql_error_code("CONNECT TO FOREIGN DATA SOURCE",1429).
  mysql_error_code("QUERY ON FOREIGN DATA SOURCE",1430).
  mysql_error_code("FOREIGN DATA SOURCE DOESNT EXIST",1431).
  mysql_error_code("FOREIGN DATA STRING INVALID CANT CREATE",1432).
  mysql_error_code("FOREIGN DATA STRING INVALID",1433).
  mysql_error_code("CANT CREATE FEDERATED TABLE",1434).
  mysql_error_code("TRG IN WRONG SCHEMA",1435).
  mysql_error_code("STACK OVERRUN NEED MORE",1436).
  mysql_error_code("TOO LONG BODY",1437).
  mysql_error_code("WARN CANT DROP DEFAULT KEYCACHE",1438).
  mysql_error_code("TOO BIG DISPLAYWIDTH",1439).
  mysql_error_code("XADUPID",1440).
  mysql_error_code("DATETIME FUNCTION OVERFLOW",1441).
  mysql_error_code("CANT UPDATE USED TABLE IN SF OR TRG",1442).
  mysql_error_code("VIEW PREVENT UPDATE",1443).
  mysql_error_code("PS NO RECURSION",1444).
  mysql_error_code("SP CANT SET AUTOCOMMIT",1445).
  mysql_error_code("MALFORMED DEFINER",1446).
  mysql_error_code("VIEW FRM NO USER",1447).
  mysql_error_code("VIEW OTHUSER",1448).
  mysql_error_code("NO SUCH USER",1449).
  mysql_error_code("FORBID SCHEMA CHANGE",1450).
  mysql_error_code("ROW IS REFERENCED 2",1451).
  mysql_error_code("NO REFERENCED ROW 2",1452).
  mysql_error_code("SP BAD VAR SHADOW",1453).
  mysql_error_code("TRG NO DEFINER",1454).
  mysql_error_code("OLD FILE FORMAT",1455).
  mysql_error_code("SP RECURSION LIMIT",1456).
  mysql_error_code("SP PROC TABLE CORRUPT",1457).
  mysql_error_code("SP WRONG NAME",1458).
  mysql_error_code("TABLE NEEDS UPGRADE",1459).
  mysql_error_code("SP NO AGGREGATE",1460).
  mysql_error_code("MAX PREPARED STMT COUNT REACHED",1461).
  mysql_error_code("VIEW RECURSIVE",1462).
  mysql_error_code("NON GROUPING FIELD USED",1463).
  mysql_error_code("TABLE CANT HANDLE SPKEYS",1464).
  mysql_error_code("NO TRIGGERS ON SYSTEM SCHEMA",1465).
  mysql_error_code("REMOVED SPACES",1466).
  mysql_error_code("AUTOINC READ FAILED",1467).
  mysql_error_code("USERNAME",1468).
  mysql_error_code("HOSTNAME",1469).
  mysql_error_code("WRONG STRING LENGTH",1470).
  mysql_error_code("NON INSERTABLE TABLE",1471).
  mysql_error_code("ERROR LAST",1471).

}
