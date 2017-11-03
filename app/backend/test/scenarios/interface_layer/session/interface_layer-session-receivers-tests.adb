with Shared.Process_Types;

-- library
with GNAT.Sockets;
with AUnit.Assertions;

-- core
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

package body Interface_Layer.Session.Receivers.Tests is
   package Ass renames AUnit.Assertions;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------

   procedure Set_Up (T: in out Receivers_Test) is
   begin
      null;
   end Set_Up;

   procedure Tear_Down (T: in out Receivers_Test) is
   begin
      null;
   end Tear_Down;

   procedure Tear_Down_Case (T: in out Receivers_Test)
   is
      procedure Free is new  Ada.Unchecked_Deallocation (Receiver, Reference);
   begin
      -- terminate the Receiver task
      Receiver_State := PT.TERMINATED;
      if (Receiver_Ref /= NULL)
      then
         -- free resources
         GNAT.Sockets.Close_Socket (Server);
         Free (Receiver_Ref);
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
      Ass.Assert ((Receiver_State = Shared.Process_Types.TERMINATED),
         "Receiver not in TERMINATED state");
      Receivers.Init (Hostname, Port, 1);
      Ass.Assert ((Receiver_State = Shared.Process_Types.READY),
         "Receiver not in READY state");
      Ass.Assert ((GNAT.Sockets.Image (Address) = Expected_Addr),
         "Incorrect Socket Address");
      Ass.Assert (
      (GNAT.Sockets.Image
         (GNAT.Sockets.Get_Socket_Name (Server)) = Expected_Addr),
         "Incorrect Server listening to an incorrect Address");
      Ass.Assert ((Receiver_Ref /= NULL),
         "Receiver Reference is NULL");
   end Test_Init;

   procedure Test_Start (T: in out TC.Test_Case'Class)
   is
      use Shared.Process_Types; -- make /= visible
      Hostname : String := "127.0.0.1";
      Port : GNAT.Sockets.Port_Type := 9000;
      Expected_Port : String :=
         Ada.Strings.Fixed.Trim (Port'Img, Ada.Strings.Left);
      Expected_Addr : String := Hostname & ":" & Expected_Port;
   begin
   -- Reproduce Init
      Stack_Instance.Init (1);
      Address.Addr := G_Socket.Addresses (G_Socket.Get_Host_By_Name (Hostname), 1);
      Address.Port := Port;
      G_Socket.Create_Socket (Socket => Server);
      G_Socket.Set_Socket_Option
      (Socket => Server,
       Option => (Name    => G_Socket.Reuse_Address, Enabled => True));
      G_Socket.Bind_Socket
      (Socket  => Server,
      Address => (Family => G_Socket.Family_Inet,
                      Addr   => Address.Addr,
                      Port   => Address.Port));
      G_Socket.Listen_Socket (Socket => Server);
      Receiver_State := PT.READY;
      -- create the receiver task
      Receiver_Ref := new Receiver;
   -- Test Start
      Receivers.Start;
      Ass.Assert ((Receiver_State = Shared.Process_Types.ACTIVE),
         "Receiver not in ACTIVE state");
   end Test_Start;

   procedure Test_Shutdown (T : in out TC.Test_Case'Class)
   is
   begin
      -- TODO: implement this test
      Ass.Assert (0=1,
                  "Test not yet implemented");
      null;
   end Test_Shutdown;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Receivers_Test) is
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

   function Name(T: Receivers_Test) return AU.Message_String is
   begin
      return AU.Format ("Receivers");
   end Name;
end Interface_Layer.Session.Receivers.Tests;
