/*----------------------------------------------------------------------------
   Copyright (C) Microsoft Corporation
  
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

import sys/node/tiersplit
import sys/dom

type clientCommand { 
  Display( message: string ) 
}

type serverCommand { 
  Utter( message :string ) 
  SetUsername( uname :string ) 
}

function xlog( s : string ) {
  trace( "[log] " + s );
}

public function main() : io ()
{    
  function serverCode()         
  { 
    xlog("moebius server listening on localhost:8080")

    // exit node server if the user presses enter
    println("press enter to exit..")
    readln() fun(s) {
      exit(0);
    }
    
    val broadcast = createBroadcast()
    
    function serverSession(client)
    { 
      xlog("`serverCode.serverSession` is executed") 

      var username := "anonymous"

      Display("|  Welcome to tierSplitted moebius chat. Have fun!").client
      Display("|     (use `/nick` to change your username)\n").client
      broadcast.listen fun(s) {
        Display(s).client 
      }

      // handle messages
      function handler(c) {
        match(c) { 
          Utter(message) -> { 
            broadcast.emit( username + ": " + message )
          }
          SetUsername(name) -> { 
            Display("/nick " + name).client
            broadcast.emit( "*** `" + username + "` is now known as `" + name + "`" )
            username := name                     
          }
        }
      }

      return handler;
    }

    return serverSession
  }

  
  function clientCode() { 
    xlog("`clientCode` is executed")

    function clientSession(server) { 
      xlog("`clientCode.clientSession` is executed")
      
      function loop() { 
        readln() fun(input) {
          if( input.startsWith("/") ) {
            if( input.startsWith("/nick ") ) {
              server(SetUsername( input.substr(6) ))
            } 
            else {
              println( ">>> unknown command " )
            }
          } 
          else {
            Utter(input).server
          }
          loop()
        }
      }
      loop()

      return fun(c) {
        match(c) {
          Display( message ) -> {
            println( message )
          }
        }
      }
    }

    return clientSession
  }

  nodejsMoebius( serverCode, clientCode )
}
