--  This code extracted from file dot5c.tex by Latex.

------------------------------------------------------------------------------
--                                                                          --
--   POSIX Ada95 Bindings for Protocol Independent Interfaces (P1003.5c)    --
--                                                                          --
--                   P O S I X . S o c k e t s . L o c a l                  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                                                                          --
--  This file is part of an implementation of an Ada95 API for the sockets  --
--  and network support services found in P1003.1g -- Protocol Independent  --
--  Interfaces.  It is integrated with the  FSU Implementation of POSIX.5b  --
--  (FLORIST), an Ada API for  POSIX OS services for use with the GNAT Ada  --
--  compiler and the FSU Gnu Ada Runtime Library (GNARL).                   --
--                                                                          --
--  This package specification contains some text extracted from  IEEE STD  --
--  1003.5: 1990, Information Technology --  POSIX Ada Language Interfaces  --
--  Part 1:  Binding for System Application Program Interface,  as amended  --
--  by IEEE STD 1003.5b: 1996,  Amendment 1: Realtime  Extensions,  and as  --
--  further amended by IEEE Draft STD 1003.5c: 1997, Amendment 2: Protocol  --
--  Independent Interfaces,  copyright 1997 by the Institute of Electrical  --
--  and Electronics Engineers, Inc.                                         --
--                                                                          --
--  The package specifications in the IEEE standards cited above represent  --
--  only a  portion  of  the  documents  and  are  not to be interpreteted  --
--  outside the context  of  the documents.  The standards must be used in  --
--  conjunction  with  the  package   specifications  in  order  to  claim  --
--  conformance.   The IEEE takes no responsibility for and will assume no  --
--  liability for damages resulting from the reader's misinterpretation of  --
--  said  information resulting from its out-of-context nature.   To order  --
--  copies of the IEEE standards,  please contact the  IEEE Service Center  --
--  at 445 Hoes Lane, PO Box 1331, Piscataway, NJ 08855-1331; via phone at  --
--  1-800-678-IEEE, 908-981-1393; or via fax at 908-981-9667.               --
--                                                                          --
--  These  package  specifications are  distributed in  the hope that they  --
--  will  be useful, but  WITHOUT  ANY  WARRANTY; without even the implied  --
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.        --
--                                                                          --
------------------------------------------------------------------------------

with POSIX.C;
package POSIX.Sockets.Local is

   Local_Protocol : constant Protocol_Family := PF_LOCAL;
   type Local_Socket_Address is new Socket_Address with private;
   function Get_Socket_Path (Name : Local_Socket_Address)
     return POSIX.Pathname;
   procedure Set_Socket_Path
      (Name : in out Local_Socket_Address;
       Path : in     POSIX.Pathname);
   --  Dispatching operations for Local_Socket_Address
   function Get_Socket_Name (Handle : Socket_Message)
     return Local_Socket_Address;
   function Get_Address (Info_Item : Socket_Address_Information)
     return Local_Socket_Address;
   function Get_Peer_Name (Socket : POSIX.IO.File_Descriptor)
     return Local_Socket_Address;
   function Get_Socket_Name (Socket : POSIX.IO.File_Descriptor)
     return Local_Socket_Address;

private
   
   use POSIX.C.Sockets;

   type Local_Socket_Address is new Socket_Address with record
      C : aliased POSIX.C.Sockets.struct_sockaddr_un :=
         struct_sockaddr_un ' (sun_family => AF_LOCAL,
                               sun_path   => (others => NUL));
   end record;

   function Address (Name : Local_Socket_Address)
     return POSIX.C.Sockets.sockaddr_var_ptr;

   function Length (Name : Local_Socket_Address)
     return POSIX.C.size_t;

end POSIX.Sockets.Local;
