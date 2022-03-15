------------------------------------------------------------------------------
--                                                                          --
--                      POSIX.5b VALIDATION TEST SUITE                      --
--                                                                          --
--                             P 0 4 0 3 0 0                                --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                                                                          --
--  Copyright (c) 1995-1998 Florida  State  University  (FSU).  All Rights  --
--  Reserved.                                                               --
--                     Copyright (C) 1999-2022, AdaCore                     --
--                                                                          --
--  This is free software;  you can redistribute it and/or modify it under  --
--  terms of the  GNU  General  Public  License  as published by the  Free  --
--  Software Foundation;  either version 2, or (at your option) any  later  --
--  version.  This  software  is distributed  in the hope that it  will be  --
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty of  --
--  MERCHANTABILITY   or  FITNESS FOR A PARTICULAR PURPOSE.   See the  GNU  --
--  General Public License for more details.  You  should have  received a  --
--  copy of the GNU General Public License  distributed  with  GNARL;  see  --
--  file  COPYING.  If not,  write to  the  Free  Software  Foundation, 59  --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.                   --
--                                                                          --
--  Under contract  GS-35F-4506G, the U. S. Government obtained  unlimited  --
--  rights in the software and documentation contained herein.   Unlimited  --
--  rights are defined in DFAR 252,227-7013(a)(19).  By making this public  --
--  release,   the  Government  intends  to  confer  upon  all  recipients  --
--  unlimited  rights equal to those held by the Government.  These rights  --
--  include rights to use,  duplicate,  release  or  disclose the released  --
--  data an computer software  in whole or in part,  in any manner and for  --
--  any purpose whatsoever, and to have or permit others to do so.          --
--                                                                          --
--  DISCLAIMER   --   ALL MATERIALS OR INFORMATION HEREIN RELEASED,   MADE  --
--  AVAILABLE OR DISCLOSED ARE AS IS.   THE GOVERNMENT MAKES NO EXPRESS OR  --
--  IMPLIED WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS  --
--  OF THE SOFTWARE,  DOCUMENTATION  OR  OTHER INFORMATION RELEASED,  MADE  --
--  AVAILABLE OR DISCLOSED,  OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS  --
--  FOR A PARTICULAR PURPOSE OF SAID MATERIAL.                              --
--                                                                          --
------------------------------------------------------------------------------
--  [$Revision$]

--  Test package POSIX_Process_Environment and operations on
--  current working directory.

--  Setup: Run this test with envionment variable PWD set to ".".
--  If you are using "sh": "PWD=`pwd`; export PWD".

with POSIX,
     POSIX_Files,
     POSIX_Process_Environment,
     POSIX_Report;
procedure p040300 is

   use POSIX,
       POSIX_Files,
       POSIX_Process_Environment,
       POSIX_Report;

begin

   Header ("p040300");
   Test ("package POSIX_Process_Environment [4.3]");

   Test ("Process Working Directory [4.3.3]");
   declare
      Owd : POSIX_String := Get_Working_Directory;
   begin

      Assert (Owd'First = 1, "A001");
      Comment ("working directory = " & To_String (Owd));
      --  Check that PWD is exported
      Assert (Owd = Environment_Value_Of ("PWD"), "A002");
      Comment ("PWD = " & To_String (Environment_Value_Of ("PWD")));
      Create_Directory ("testdir", (others => True));

      Assert (Is_Directory ("testdir"), "A003");

      Change_Working_Directory ("testdir");
      Assert (not Is_Directory ("testdir"), "A004");

      begin
         Change_Working_Directory ("Nonexistent_Directory");
         Expect_Exception ("A005");
      exception
      when POSIX_Error => Check_Error_Code (No_Such_File_Or_Directory, "A006");
      end;

      declare
         Newcwd : POSIX_String := Get_Working_Directory;
      begin
         Assert (Newcwd'First = 1, "A007");
         Comment ("working directory = " & To_String (Newcwd));
         Assert (Newcwd = Owd & "/testdir", "A008");
      end;

      Change_Working_Directory ("..");
      Remove_Directory ("testdir");
   exception when E : others => Unexpected_Exception (E, "A009");
   end;

   Test ("Environment Variables [4.3.2]");

   declare
      type POSIX_String_Ptr is access all POSIX_String;
      type Env_Val is record
         Name  : POSIX_String_Ptr;
         Value : POSIX_String_Ptr;
      end record;
      type Env_Vals is array (Positive range <>) of Env_Val;
      --  Try A String Whose Lower Bound is Not One.
      Skxsjl : aliased POSIX_String := "Skxsjl";
      --  Note : The Following Hack is Necessary Because Gnat 2.00 Doesn'T
      --  Like New' ("").  We Should Verify That The Problem is Gone In 2.02.
      Empty_String : aliased POSIX_String := "";
      Data : constant Env_Vals := (
         (new POSIX_String'("Xxx"), new POSIX_String'("Value Of Xxx")),
         (new POSIX_String'("Abcdefg"),
          new POSIX_String'("Value Of Abcdefg")),
         (new POSIX_String'("Empty_Value"), Empty_String'Access),
         (Skxsjl'Access, Skxsjl'Access),
         (new POSIX_String'("Long_Name_with_A_Very_Long_Value"),
          new POSIX_String'("This is The Value Associated with The Very Lo" &
          "ng Name.  I Expect That The Length Of The Name And Value is Rea" &
          "lly Irrelevant, But It Can'T Hurt To Stress The Size Limits A B" &
          "it.")),
         (new POSIX_String'("Last_Name"), new POSIX_String'("Xxx")));

      procedure Add_Vals (Vals : Env_Vals; Env : in out Environment);

      procedure Add_Vals (Vals : Env_Vals; Env : in out Environment) is
      begin
         for I in Vals'Range loop
            Set_Environment_Variable (Vals (I).Name.all,
            Vals (I).Value.all, Env);
         end loop;
      end Add_Vals;

      procedure Check_Vals (Vals : Env_Vals; Env : in out Environment);

      procedure Check_Vals (Vals : Env_Vals; Env : in out Environment) is
      begin
         for I in Vals'Range loop
            if not Is_Environment_Variable (Vals (I).Name.all, Env) then
               --  Vals (I).Name.all is Missing From Environment
               Fail ("A010");
            end if;
            --  Check Vals of I
            Assert (Environment_Value_Of (Vals (I).Name.all, Env) =
                   Vals (I).Value.all, "A011");
         end loop;
      end Check_Vals;

      procedure Print_Val
        (Name : POSIX_String; Value : POSIX_String; Quit : in out Boolean);

      procedure Print_Val
        (Name : POSIX_String; Value : POSIX_String; Quit : in out Boolean)
      is
         pragma Unreferenced (Quit);
      begin
         Comment (To_String  (Name) & "=" & To_String (Value));
         Assert (Name'First = 1, "A012");
         Assert (Value'First = 1, "A013");
      end Print_Val;

      procedure Print_Env is new For_Every_Environment_Variable (Print_Val);
      Env1, Env2, Env3 : Environment;
      Initial_Env : Environment;

   begin
      --  First Verify Basic Sanity.
      Set_Environment_Variable ("ABC", "abc");
      Assert (Environment_Value_Of ("ABC") = "abc", "A014");
      Assert (Environment_Value_Of ("undefined_variable") = "", "A015");
      Assert (Environment_Value_Of ("undefined_variable", "XXX")
               = "XXX", "A016");
      Assert (Is_Environment_Variable ("ABC"), "A017");
      Assert (not Is_Environment_Variable ("undefined_variable"), "A018");

      Assert (Length (Env1) = 0, "A019");
      Assert (not Is_Environment_Variable ("ABC", Env1), "A020");
      Add_Vals (Data, Env1);
      Print_Env (Env1);
      Assert (Length (Env1) = Data'Length, "A021");
      Check_Vals (Data, Env1);
      Copy_Environment (Env1, Env2);
      Assert (Length (Env2) = Data'Length, "A022");
      Check_Vals (Data, Env2);
      Check_Vals (Data, Env1);
      Assert (Is_Environment_Variable ("Abcdefg", Env1), "A023");
      Delete_Environment_Variable ("Abcdefg", Env1);
      Print_Env (Env1);
      Assert (Length (Env1) = Data'Length - 1, "A024");
      Assert (not Is_Environment_Variable ("Abcdefg", Env1), "A025");
      Assert (Environment_Value_Of ("Abcdefg", Env1, "X") = "X", "A026");
      Delete_Environment_Variable ("Abcdefg", Env1);
      Print_Env (Env1);
      Assert (Length (Env1) = Data'Length - 1, "A027");
      Add_Vals (Data, Env1);
      Print_Env (Env1);
      Assert (Length (Env1) = Data'Length, "A028");
      Check_Vals (Data, Env1);
      Clear_Environment (Env1);
      Assert (Length (Env1) = 0, "A029");
      Assert (not Is_Environment_Variable ("Abcdefg", Env1), "A030");
      Copy_From_Current_Environment (Initial_Env);
      Assert (Is_Environment_Variable ("ABC", Initial_Env), "A031");
      Assert (Environment_Value_Of ("ABC", Initial_Env) = "abc", "A032");
      Add_Vals (Data, Env1);
      Copy_Environment (Initial_Env, Env1);
      Assert (Environment_Value_Of ("ABC", Env1) = "abc", "A033");
      Assert (not Is_Environment_Variable ("Abcdefg", Env1), "A034");
      Add_Vals (Data, Env1);
      Check_Vals (Data, Env1);
      Assert (Environment_Value_Of ("ABC", Env1) = "abc", "A035");
      Clear_Environment;
      Assert (POSIX_Process_Environment.Length = 0, "A036");
      Add_Vals (Data, Env3);
      Assert (Length (Env3) = Data'Length, "A037");
      Check_Vals (Data, Env3);
      Set_Environment_Variable ("Aaa", "Bbb");
      Assert (Environment_Value_Of ("Aaa") = "Bbb", "A038");
      --  Copy_To_Current_Environment (env3);
      --  test died here strangly. need to check later.
      Delete_Environment_Variable ("Aaa");
      Set_Environment_Variable ("Xxx", "Value of Xxx");
      Set_Environment_Variable ("Abcdefg", "Value of Abcdefg");
      Set_Environment_Variable ("Empty_Value", "");
      Set_Environment_Variable ("Sjkrls", "Value Of Sjkrls");
      Set_Environment_Variable ("Skxsjl", "Skxsjl");
      Set_Environment_Variable ("Long_Name_with_A_Very_Long_Value",
         "This is The Value Associated with The Very Long" &
         " Name.  I Expect That The Length Of The Name And Value is Really " &
         "Irrelevant, But It Can'T Hurt To Stress The Size Limits A Bit");
      Set_Environment_Variable ("Last_Name", "Xxx");
      Assert (Is_Environment_Variable (Skxsjl), "A039");
      Delete_Environment_Variable (Skxsjl);
      Assert (not Is_Environment_Variable (Skxsjl), "A040");

      Comment ("iterator on environment");
      declare
         Found : Boolean := False;
         procedure Check_Abcdefg
           (Name : POSIX_String; Value : POSIX_String; Quit : in out Boolean);
         procedure Check_Abcdefg
           (Name : POSIX_String; Value : POSIX_String; Quit : in out Boolean)
         is
            pragma Unreferenced (Value);
         begin
            if Found then
               --  Quit didn't work
               Fail ("A041");
            end if;
            if Name = "Abcdefg" then
               Found := True;
               Quit := True;
            end if;
         end Check_Abcdefg;

         procedure Check is new
            For_Every_Current_Environment_Variable (Check_Abcdefg);
      begin
         Check;
         Assert (Found, "A042");
      end;

      Comment ("variable name containing =");
      begin
         if Environment_Value_Of ("A=B") = "" then
            Fail ("A043");
         end if;
         Expect_Exception ("A044");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A045");
      end;

      Comment ("variable name containing NUL");
      declare
         Nul : POSIX_Character := POSIX_Character'Val (0);
      begin
         Set_Environment_Variable ("A" & Nul, "Xxx");
         Expect_Exception ("A046");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A047");
      end;

      Comment ("variable name containing =");
      begin
         Delete_Environment_Variable ("A=B");
         Expect_Exception ("A048");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A049");
      end;

      Comment ("null string as variable");
      begin
         Delete_Environment_Variable ("");
         Expect_Exception ("A050");
      exception
      when POSIX_Error => Check_Error_Code (Invalid_Argument, "A051");
      end;

      Comment ("Copy_To_Current_Environment");
      Copy_To_Current_Environment  (Initial_Env);
      Assert (Environment_Value_Of  ("ABC") = "abc", "A052");
      Assert (POSIX_Process_Environment.Length
        = Length  (Initial_Env), "A053");

   exception
   when E : others => Unexpected_Exception (E, "A054");
   end;

   Done;

exception
when E : others => Fatal_Exception (E, "A055");
end p040300;
