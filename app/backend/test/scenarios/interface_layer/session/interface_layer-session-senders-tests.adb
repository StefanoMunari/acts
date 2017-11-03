with Shared.Process_Types;

-- library
with GNAT.Sockets;
with AUnit.Assertions;

-- core
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

package body Interface_Layer.Session.Senders.Tests is
   package Ass renames AUnit.Assertions;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------

   procedure Set_Up (T: in out Senders_Test) is
   begin
      null;
   end Set_Up;

   procedure Tear_Down (T: in out Senders_Test) is
   begin
      null;
   end Tear_Down;

   procedure Tear_Down_Case (T: in out Senders_Test)
   is
      procedure Free is new  Ada.Unchecked_Deallocation (Sender, Reference);
   begin
      -- terminate the sender task
      Sender_State := PT.TERMINATED;
      if (Sender_Ref /= NULL)
      then
         -- free resources
         Free (Sender_Ref);
      end if;
   end Tear_Down_Case;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Init (T : in out TC.Test_Case'Class)
   is
      use Shared.Process_Types; -- make /= visible
      Hostname : String := "127.0.0.1";
      Port : GNAT.Sockets.Port_Type := 9000;
      Expected_Port : String :=
         Ada.Strings.Fixed.Trim (Port'Img, Ada.Strings.Left);
      Expected_Addr : String := Hostname & ":" & Expected_Port;
   begin
      Ass.Assert ((Sender_State = Shared.Process_Types.TERMINATED),
         "Sender not in TERMINATED state");
      Senders.Init (Hostname, Port);
      Ass.Assert ((Sender_State = Shared.Process_Types.READY),
         "Sender not in READY state");
      Ass.Assert ((GNAT.Sockets.Image (Server_Socket) = Expected_Addr),
         "Incorrect Socket Address");
      Ass.Assert ((Sender_Ref /= NULL),
         "Sender Reference is NULL");
   end Test_Init;

   procedure Test_Start (T : in out TC.Test_Case'Class)
   is
      use Shared.Process_Types; -- make /= visible
      Hostname : String := "127.0.0.1";
      Port : GNAT.Sockets.Port_Type := 9000;
   begin
   -- Reproduce Init
      Server_Socket.Addr :=
         GNAT.Sockets.Addresses (GNAT.Sockets.Get_Host_By_Name (Hostname), 1);
      Server_Socket.Port := Port;
      Sender_State := Shared.Process_Types.READY;
      -- create the sender task
      Sender_Ref := new Sender;
   -- Test Start
      Senders.Start;
      Ass.Assert ((Sender_State = Shared.Process_Types.ACTIVE),
         "Sender not in ACTIVE state");
   end Test_Start;

   procedure Test_Shutdown (T : in out TC.Test_Case'Class)
   is
      use Shared.Process_Types; -- make /= visible
      Hostname : String := "127.0.0.1";
      Port : GNAT.Sockets.Port_Type := 9000;
   begin
   -- Reproduce Init
      Server_Socket.Addr :=
         GNAT.Sockets.Addresses (GNAT.Sockets.Get_Host_By_Name (Hostname), 1);
      Server_Socket.Port := Port;
      Sender_State := Shared.Process_Types.READY;
      -- create the sender task
      Sender_Ref := new Sender;
   -- Reproduce Start
      Sender_State := PT.ACTIVE;
      Sender_Ref.all.Send;
   -- Test Shutdown
      Senders.Shutdown;
      Ass.Assert ((Sender_State = Shared.Process_Types.TERMINATED),
         "Sender not in TERMINATED state");
      Ass.Assert ((Sender_Ref = NULL),
         "Sender Reference not freed");
   end Test_Shutdown;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Senders_Test) is
      use TC.Registration;
   begin
      Register_Routine (Test => T,
                        Routine => Test_Init'Access,
                        Name => "Test execute Init");
      Register_Routine (Test => T,
                        Routine => Test_Start'Access,
                        Name => "Test execute Start");
      Register_Routine (Test => T,
                        Routine => Test_Shutdown'Access,
                        Name => "Test execute Shutdown");
   end Register_Tests;

   function Name(T: Senders_Test) return AU.Message_String is
   begin
      return AU.Format ("Senders");
   end Name;
end Interface_Layer.Session.Senders.Tests;
