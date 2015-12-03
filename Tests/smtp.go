smtp{
  import go.smtp.
  import go.io.
  import go.unit.

  smtpTest:[string]@=harness.
  smtpTest(H)..{
    doAction() ->
	Email = email("frankmccabe@mac.com","This is a test from Go",[]);
	config(H,"mac.com","frankmccabe");
	Email.post().
  }.
}