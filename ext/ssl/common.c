/*
 * Common functions to access the ssl library
 * Part of the ssl extension to Go!
 */

#include "config.h"        /* pick up standard configuration header */
#include <go.h>
#include <pool.h>
#include <dll.h>                /* dynamic loader interface */

#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>
#include <string.h>

#include <openssl/ssl.h>


BIO *bio_err=NULL;
static void sigpipe_handle(int x)
{
}

/* Print SSL errors and exit*/
int berr_exit(char *string)
{
  BIO_printf(bio_err,"%s\n",string);
  ERR_print_errors(bio_err);
  exit(0);
}


SSL_CTX *initialize_ctx(char *keyfile,char *password)
{
  SSL_METHOD *meth;
  SSL_CTX *ctx;
    
  if(!bio_err){
    /* Global system initialization*/
    SSL_library_init();
    SSL_load_error_strings();
      
    /* An error write context */
    if(logFile!=NULL)
      bio_err = BIO_new_fd(fileData(logFile),BIO_NOCLOSE);
    else
      bio_err=BIO_new_fp(2,BIO_NOCLOSE); /* Standard error channel */
  }

  /* Set up a SIGPIPE handler */
  signal(SIGPIPE,sigpipe_handle);
    
  /* Create our context*/
  meth=SSLv23_method();
  ctx=SSL_CTX_new(meth);

  /* Load our keys and certificates*/
  if(!(SSL_CTX_use_certificate_chain_file(ctx,keyfile)))
    berr_exit("Can't read certificate file");

  pass=password;
  SSL_CTX_set_default_passwd_cb(ctx,password_cb);
  if(!(SSL_CTX_use_PrivateKey_file(ctx,keyfile,SSL_FILETYPE_PEM)))
    berr_exit("Can't read key file");

  /* Load the CAs we trust*/
  if(!(SSL_CTX_load_verify_locations(ctx,CA_LIST,0)))
    berr_exit("Can't read CA list");
#if (OPENSSL_VERSION_NUMBER < 0x00905100L)
  SSL_CTX_set_verify_depth(ctx,1);
#endif
  return ctx;
}
     
