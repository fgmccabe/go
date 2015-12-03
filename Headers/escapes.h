/* 
  This is where you define a new escape function so that the compiler and
  the run-time system can see it
  (c) 2001-2002 F.G.McCabe

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  Contact: Francis McCabe <frankmccabe@mac.com>

 */

/* Declare standard symbols and constructors */

#define processState "U'#processState'\0"
#define errorType "U'#exception'\0"
#define threadType "U'#thread'\0"
#define thingType "U'#thing'\0"
#define fileType "U'go.io#fileType'\0"
#define filePerm "U'go.io#filePerm'\0"
#define debugType "U'go.debug#debugger'\0"
#define tplType "U'#,'\2"

  /* Define the standard escapes */
  escape("exit",g_exit,0,True,False,"p\1+i","terminate go engine")
  escape("__command_line",g_command_line,1,False,False,"F\0LS","command line arguments")
  escape("__command_opts",g_commandOptions,3,False,False,"F\0L"tplType"cs","command line options")

  escape("=",g_equal,4,False,False,":\0AP\2?$\0?$\0","unification")
  escape("==",g_ident,5,False,False,":\0AP\2+$\0+$\0","test for identicality")
  escape(".=",g_match,6,False,False,":\0AP\2+$\0?$\0","matching")

  escape("var",g_var,7,False,False,":\1AP\1+$\1","test for variable")
  escape("nonvar",g_nonvar,8,False,False,":\1AP\1+$\1","test for non-variable")

  escape("__errorcode",g_errorcode,10,False,False,"F\1+sS","decode error symbol")

  escape("__call",g_call,11,True,False,"p\4+s+s+i+LS","dynamic call")

  escape("__defined",g_defined,12,True,False,"P\3+s+s+i","test for defined name")

  escape("+",g_plus,13,False,False,":\1NF\2+$\1+$\1$\1","add two numbers")
  escape("-",g_minus,14,False,False,":\1NF\2+$\1+$\1$\1","subtract two numbers")
  escape("*",g_times,15,False,False,":\1NF\2+$\1+$\1$\1","multiply two numbers")
  escape("/",g_div,16,False,False,":\1NF\2+$\1+$\1f","divide two numbers")
  escape("iplus",g_iplus,17,False,False,"F\3+i+i+ii","modulo addition")
  escape("iminus",g_iminus,18,False,False,"F\3+i+i+ii","modulo subtraction")
  escape("itimes",g_itimes,19,False,False,"F\3+i+i+ii","modulo multiplication")
  escape("idiv",g_idiv,20,False,False,"F\3+i+i+ii","modulo division")
  escape("imod",g_imod,21,False,False,"F\2+i+ii","modulo remainder")
  escape("quot",g_quot,22,False,False,":\1NF\2+$\1+$\1i","integer quotient")
  escape("rem",g_rem,23,False,False,":\1NF\2+$\1+$\1f","remainder")
  escape("abs",g_abs,24,False,False,":\1NF\1+$\1$\1","absolute value")
  escape("**",g_pow,25,False,False,":\1NF\2+$\1+$\1$\1","raise X to the power Y")
  escape("sqrt",g_sqrt,26,False,False,":\1NF\1+$\1f","square root")
  escape("exp",g_exp,27,False,False,":\1NF\1+$\1$\1","exponential")
  escape("log",g_log,28,False,False,":\1NF\1+$\1f","logarithm")
  escape("log10",g_log10,29,False,False,":\1NF\1+$\1f","10-based logarithm")
  escape("pi",g_pi,30,False,False,"F\0f","return PI")
  escape("sin",g_sin,31,False,False,":\1NF\1+$\1f","sine")
  escape("cos",g_cos,32,False,False,":\1NF\1+$\1f","cosine")
  escape("tan",g_tan,33,False,False,":\1NF\1+$\1f","tangent")
  escape("asin",g_asin,34,False,False,":\1NF\1+$\1f","arc sine")
  escape("acos",g_acos,35,False,False,":\1NF\1+$\1f","arc cosine")
  escape("atan",g_atan,36,False,False,":\1NF\1+$\1f","arc tangent")

  escape("srand",g_srand,37,False,False,":1Np\1+$\1","set random seed")
  escape("rand",g_rand,38,False,False,":\1NF\1+$\1f","random # generator")
  escape("irand",g_irand,39,False,False,"F\1+ii","generate random integer")

  escape("ldexp",g_ldexp,40,False,False,"F\2+N+Nf","raise x to 2**y")
  escape("frexp",g_frexp,41,False,False,"P\3+f-f-i","split x into mant and exp")
  escape("modf",g_modf,42,False,False,"P\3+f-f-f","split x into int and frac")

  escape("band",g_band,43,False,False,"F\2+i+ii","bitwise and two numbers")
  escape("bor",g_bor,44,False,False,"F\2+i+ii","bitwise or two numbers")
  escape("bxor",g_bxor,45,False,False,"F\2+i+ii","bitwise xor two numbers")
  escape("bleft",g_bleft,46,False,False,"F\2+i+ii","bitwise left shift")
  escape("bright",g_bright,47,False,False,"F\2+i+ii","bitwise right shift")
  escape("bnot",g_bnot,48,False,False,"F\1+ii","bitwise negate number")

  escape("trunc",g_trunc,49,False,False,":\1NF\1+$\1$\1","truncate to nearest integer")
  escape("floor",g_floor,50,False,False,":\1NF\1+$\1$\1","truncate to lower integer")
  escape("ceil",g_ceil,51,False,False,"F\1+Ni","truncate to next integer")
  escape("itrunc",g_itrunc,52,False,False,"F\1+Ni","truncate to 64 bit integer")
  escape("integral",g_integral,53,False,False,"P\1+N","test if number is integral")
  escape("n2float",g_float,54,False,False,":\1NF\1+$\1f","float a number")
  
  escape("<",g_less,55,False,False,":\0AP\2+$\0+$\0","compare terms")
  escape("=<",g_le,56,False,False,":\0AP\2+$\0+$\0","compare terms")
  escape(">",g_gt,57,False,False,":\0AP\2+$\0+$\0","compare terms")
  escape(">=",g_ge,58,False,False,":\0AP\2+$\0+$\0","compare terms")
  
  escape("ground",g_ground,59,False,False,":\0AP\1+$\0","test for grounded")

  escape("__suspend",g_suspend,64,False,False,":\1A:\2AP\2+$\1+$\2","suspend if variable not bound")

  escape("__assert",g_assert,70,False,False,":\1Ap\2+s+$\1","assert a term")
  escape("__retract",g_retract,71,False,False,"p\1+s","remove assertion")

  escape("__term",g_term,75,False,False,":\1AF\1+$\1s","define an assertion")
  escape("__is",g_is,76,False,False,":\1AP\2+s+$\1","invoke an assertion")
  escape("__remove",g_remove,77,False,False,"p\1+s","retract a definition")

  // Create a new object -- clone a term to make an object
  escape("__newObject",g_newObject,80,False,False,":\1AF\1+$\1$\1","create a new object")

  // Property management
  escape("__setProp",g_setProp,81,False,False,":\1AP\3+"thingType"+s+$\1","set a property on a symbol")
  escape("__getProp",g_getProp,82,False,False,":\1AP\3+"thingType"+s-$\1","get a symbol property")
  escape("__delProp",g_delProp,83,False,False,":\1AP\2+"thingType"+s","delete a property from a symbol")

  // Term construction
  escape("__univ",g_univ,84,False,False,":\1A:\2AF\2+s+L$\1$\2","weird function to construct terms")  

  // Lock management
  escape("__acquireLock",g_acquireLock,91,False,False,":\0AP\2+$\0+N","acquire lock")
  escape("__waitLock",g_waitLock,92,False,False,":\0AP\2+$\0+N","release and wait on a lock")
  escape("__releaseLock",g_releaseLock,93,False,False,":\0AP\1+$\0","release a lock")

  escape("__newhash",g_newhash,95,False,False,"F\1+iO","create a new hash table")
  escape("__hashsearch",g_search,96,False,False,":\2A:\3AP\3+O+$\2-$\3","search a hash table")
  escape("__hashinsert",g_insert,97,False,False,":\2A:\3AF\3+O+$\2+$\3O","add to hash table")
  escape("__hashcount",g_count,98,False,False,"F\1+Oi","count elements of hash table")
  escape("__hashdelete",g_delete,99,False,False,":\1Ap\2+O+$\1","delete element from hash table")
  escape("__hashcontents",g_contents,100,False,False,":\1AF\1+OL$\1","entire contents of hash table")
  escape("__hashkeys",g_keys,101,False,False,":\1AF\1+OL$\1","list of all keys in hash table")
  escape("__hashvalues",g_values,94,False,False,":\1AF\1+OL$\1","entire contents of hash table")
  escape("__hashterm",g_hash_term,103,False,False,":\1AP\2+$\1-i","compute hash of term")

  // Sha function
  escape("__sha1",g_sha1,102,False,False,"F\1+LiLi","compute hash of a byte string")

  escape("__openURL",g_openURL,110,True,False,"F\4+S+S+S+iO","open a URL")
  escape("__openInFile",g_openInFile,111,True,False,"F\2+S+iO","open input file")
  escape("__openOutFile",g_openOutFile,112,True,False,"F\2+S+iO","open output file")
  escape("__openAppendFile",g_openAppendFile,113,True,False,"F\2+S+iO","open output file")
  escape("__openIOAppFile",g_openAppendIOFile,114,True,False,"F\2+S+iO","open output file")
  escape("__checkRoot",g_checkRoot,115,True,False,"P\2+S+S","check url against root URL")
  escape("__mergeURL",g_mergeURL,116,True,False,"F\2+S+SS","merge URLs")
  escape("__createURL",g_createURL,117,True,False,"F\4+S+S+S+iO","create a URL")
  escape("__popen",g_popen,118,True,False,"p\7+S+LS+L"tplType"sS+O+O+O+i","open a pipe")
  
  escape("__close",g_fclose,119,True,False,"p\1+O","close file")
  escape("__eof",g_eof,120,True,False,"P\1+O","end of file test")
  escape("__ready",g_ready,121,True,False,"P\1+O","file ready test")
  escape("__inchars",g_inchars,122,True,False,"F\2+O+iS","read block string")
  escape("__inbytes",g_inbytes,123,True,False,"F\2+O+iLi","read block of bytes")
  escape("__inchar",g_inchar,124,True,False,"F\1+Oc","read single character")
  escape("__inbyte",g_inbyte,125,True,False,"F\1+Oi","read single byte")
  escape("__inline",g_inline,126,True,False,"F\2+O+SS","read a line")
  escape("__intext",g_intext,127,True,False,"F\2+O+SS","read until matching character")
  escape("__outch",g_outch,128,True,False,"p\2+O+c","write a single character")
  escape("__outbyte",g_outbyte,129,True,False,"p\2+O+i","write a single byte")
  escape("__outtext",g_outtext,130,True,False,"p\2+O+S","write a string as a block")
  escape("__outsym",g_outsym,131,True,False,"p\2+O+s","write a symbol")
  escape("__stdfile",g_stdfile,132,True,False,"F\1+iO","standard file descriptor"); 
  escape("__fposition",g_fposition,135,True,False,"F\1+Oi","report current file position")
  escape("__fseek",g_fseek,136,True,False,"p\2+O+i","seek to new file position")
  escape("__flush",g_flush,137,True,False,"p\1+O","flush the I/O buffer")
  escape("__flushall",g_flushall,138,True,False,"p\0","flush all files")
  escape("__setfileencoding",g_setfileencoding,140,True,False,"p\2+O+i", "set file encoding on file")
    
  escape("__ensure_loaded",g_classload,141,True,False,"p\4+S+s+s-Ls","load class file")

  escape("__logmsg",g_logmsg,142,False,False,"P\1+S","log a message in logfile")
  
  /* Socket handling functions */
  escape("__connect",g_connect,143,True,False,"p\5+S+i+i-O-O","connect to remote host")
  escape("__listen",g_listen,144,True,False,"p\2+i-O","listen on a port")
  escape("__accept",g_accept,145,True,False,"p\7+O-O-O-S-S-i+i","accept connection")
  //  escape("__udpPort",g_udpPort,146,True,False,"p\2NO","estabish a UDP port")
  //  escape("__udpGet",g_udpGet,147,True,False,"p\4OSSN","read a UDP datagram")
  //  escape("__udpSend",g_udpSend,148,True,False,"p\4OSSN","send a UDP datagram")
  escape("hosttoip",g_hosttoip,150,False,False,"F\1+SLS","IP address of host")
  escape("iptohost",g_iptohost,151,False,False,"F\1+SS","host name from IP")
  
  escape("__cwd",g_cwd,152,False,False,"F\0S","return current working directory")
  escape("__cd",g_cd,153,False,False,"p\1+S","change current working directory")
  escape("__rm",g_rm,154,True,False,"p\1+S","remove file")
  escape("__mv",g_mv,155,True,False,"p\2+S+S","rename file")
  escape("__mkdir",g_mkdir,156,True,False,"p\2+S+i","create directory")
  escape("__rmdir",g_rmdir,157,True,False,"p\1+S","delete directory")
  escape("__chmod",g_chmod,158,True,False,"p\2+S+i","change mode of a file or directory")
  escape("__ls",g_ls,159,True,False,"F\1+SL"tplType"S"fileType"","report on contents of a directory")
  escape("__file_present",g_file_present,160,True,False,"P\1+S","test for file")
  escape("__file_mode",g_fmode,161,True,False,"F\1+Si","report modes of a file")
  escape("__file_type",g_file_type,162,True,False,"F\1+S"fileType,"report on the type of a file")
  escape("__file_size",g_file_size,163,True,False,"F\1+Si","report on the size of a file")
  escape("__file_date",g_file_date,164,True,False,"p\4+S-f-f-f","report on file access time and modification times")

/* Timing and delaying */
  escape("delay",g_delay,170,False,False,"p\1+N","delay for period of time")  
  escape("sleep",g_sleep,171,False,False,"p\1+N","sleep until a definite time")  
  escape("now",g_now,172,False,False,"F\0f","current time")  
  escape("today",g_today,173,False,False,"F\0i","time at midnight")  
  escape("ticks",g_ticks,174,False,False,"F\0f","used CPU time")
  escape("__time2date",g_tval2date,175,False,False,"P\012+N-i-i-i-i-i-i-f-f-S", "convert a time to a date")
  escape("__time2utc",g_tval2utc,176,False,False,"P\012+N-i-i-i-i-i-i-f-f-S", "convert a time to UTC date")
  escape("__date2time",g_date2tval,177,False,False,"F\007+i+i+i+i+i+N+Nf", "convert a date to a time")
  escape("__utc2time",g_utc2tval,178,False,False,"F\010+N+N+N+N+N+N+N+Nf", "convert a UTC date to a time")

 /* Character class escapes */

  escape("__isCcChar",g_isCcChar,180,False,False,"P\1+c","is Other, control char")
  escape("__isCfChar",g_isCfChar,181,False,False,"P\1+c","is Other, format char")
  escape("__isCnChar",g_isCnChar,182,False,False,"P\1+c","is Other, unassigned char")
  escape("__isCoChar",g_isCoChar,183,False,False,"P\1+c","is Other, private char")
  escape("__isCsChar",g_isCsChar,184,False,False,"P\1+c","is Other, surrogate char")
  escape("__isLlChar",g_isLlChar,185,False,False,"P\1+c","is Letter, lowercase char")
  escape("__isLmChar",g_isLmChar,186,False,False,"P\1+c","is Letter, modifier char")
  escape("__isLoChar",g_isLoChar,187,False,False,"P\1+c","is Letter, other char")
  escape("__isLtChar",g_isLtChar,188,False,False,"P\1+c","is Letter, title char")
  escape("__isLuChar",g_isLuChar,189,False,False,"P\1+c","is Letter, uppercase char")
  escape("__isMcChar",g_isMcChar,190,False,False,"P\1+c","is Mark, spacing char")
  escape("__isMeChar",g_isMeChar,191,False,False,"P\1+c","is Mark, enclosing char")
  escape("__isMnChar",g_isMnChar,192,False,False,"P\1+c","is Mark, nonspacing char")
  escape("__isNdChar",g_isNdChar,193,False,False,"P\1+c","is Number, decimal digit")
  escape("__isNlChar",g_isNlChar,194,False,False,"P\1+c","is Number, letter char")
  escape("__isNoChar",g_isNoChar,195,False,False,"P\1+c","is Number, other char")
  escape("__isPcChar",g_isPcChar,196,False,False,"P\1+c","is Punctuation, connector")
  escape("__isPdChar",g_isPdChar,197,False,False,"P\1+c","is Punctuation, dash char")
  escape("__isPeChar",g_isPeChar,198,False,False,"P\1+c","is Punctuation, close char")
  escape("__isPfChar",g_isPfChar,199,False,False,"P\1+c","is Punctuation, final quote")
  escape("__isPiChar",g_isPiChar,200,False,False,"P\1+c","is Punctuation, initial quote")
  escape("__isPoChar",g_isPoChar,201,False,False,"P\1+c","is Punctuation, other char")
  escape("__isPsChar",g_isPsChar,202,False,False,"P\1+c","is Punctuation, open char")
  escape("__isScChar",g_isScChar,203,False,False,"P\1+c","is Symbol, currency char")
  escape("__isSkChar",g_isSkChar,204,False,False,"P\1+c","is Symbol, modifier char")
  escape("__isSmChar",g_isSmChar,205,False,False,"P\1+c","is Symbol, math char")
  escape("__isSoChar",g_isSoChar,206,False,False,"P\1+c","is Symbol, other char")
  escape("__isZlChar",g_isZlChar,207,False,False,"P\1+c","is Separator, line char")
  escape("__isZpChar",g_isZpChar,208,False,False,"P\1+c","is Separator, para char")
  escape("__isZsChar",g_isZsChar,209,False,False,"P\1+c","is Separator, space char")

  escape("__isLetterChar",g_isLetterChar,210,False,False,"P\1+c","is letter char")
  escape("__digitCode",g_digitCode,211,False,False,"F\1+ci","convert char to num")
  escape("__charOf",g_charOf,212,False,False,"F\1+ic","convert num to char")
  escape("__charCode",g_charCode,213,False,False,"F\1+ci","convert char to code")
  escape("__succChar",g_succChar,221,False,False,"F\2+c+ic","next character along")

/* String and symbol handling escapes */
  escape("int2str",g_int2str,214,False,False,"F\4+i+N+N+cS","format an integer as a string")
  escape("num2str",g_num2str,215,False,False,"F\5+N+N+N+l+lS","format a number as a string")
  escape("__stringOf",g_stringOf,216,False,False,":\0AF\3+$\0+i+iS","convert value to a string")
  escape("__trim",g_trim,217,False,False,"F\2+S+iS","trim a string to a width")
  escape("explode",g_explode,218,False,False,"F\1+sS","convert symbol to string")
  escape("implode",g_implode,219,False,False,"F\1+Ss","convert string to symbol")
  escape("gensym",g_gensym,220,False,False,"F\1+Ss","generate a new symbol")

  escape("__str2utf8",g_str2utf8,223,False,False,"F\1+SLi","encode a string as a list of utf8 bytes")
  escape("__utf82str",g_utf82str,224,False,False,"F\1+LiS","decode a list of utf8 bytes into a string")
  
  escape("getenv",g_getenv,230,False,False,"F\2+s+SS","get an environment variable")
  escape("setenv",g_setenv,231,True,False,"P\2+s+S","set an environment variable")
  escape("envir",g_envir,232,False,False,"F\0L"tplType"sS","return entire environment")
  escape("getlogin",g_getlogin,233,False,False,"F\0S","return user's login")

/* Process manipulation */
  escape("__fork",g_fork,240,False,False,"p\1+"threadType,"fork new process")
  escape("__thread",g_thread,241,False,False,"F\0"threadType"","report thread of current process")
  escape("kill",g_kill,242,True,False,"p\1+"threadType ,"kill off a process")
  escape("thread_state",g_pr_state,243,False,False,"F\1+"threadType processState"","state of process")
  escape("waitfor",g_waitfor,244,False,False,"p\1+"threadType,"wait for other thread to terminate")
  escape("__assoc",g_assoc_goal,246,False,False,":\0AP\2+$\0+p\0","associate a goal with a var")
  escape("__shell",g_shell,247,True,False,"p\4+S+LS+L"tplType"sS-i","Run a shell cmd")

  escape("__ins_debug",g_ins_debug,254,False,False,"p\0","set instruction-level")
  escape("__stackTrace",g_stackTrace,255,False,False,"p\0","Print a stack trace")

  escape("__nop",g_nop,256,False,False,"p\0","Do nothing")


#if 0
#ifdef GOXLIB
#include "xlibescapes.h"
#endif

#ifdef GOSSLIB
#include "sslEscapes.h"
#endif

#endif
