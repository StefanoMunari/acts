-- core
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Environment_Variables;
with Ada.Text_IO;

-- library
with GNAT.Sockets;
with GNATCOLL.JSON;
with GNAT.OS_Lib;

--local
with AI;
with AI.Adapter;

with Active.System;
with Active.Travel;

with Interface_Layer.Session;
with Interface_Layer.Presentation;
with Interface_Layer.Service;
with Interface_Layer.System;

with Reactive.System;

with Scheduling.System;

with Shared.Reader;
with Shared.Reader.JSON;

procedure Init is
   -- package AI_Interface renames AI_Adapter;
   package Exc renames Ada.Exceptions;
   package RJ renames Shared.Reader.JSON;
   package G_Socket renames GNAT.Sockets;
   package G_JSON renames GNATCOLL.JSON;
   package SU renames Ada.Strings.Unbounded;
   package EV renames Ada.Environment_Variables;

   File_Reader_JSON : RJ.Object;
   File_Reader      : Shared.Reader.Object'Class :=
      Shared.Reader.Object'Class (File_Reader_JSON);

-- Get city and district ids
   City_Id          : String := EV.Value (Name => "CITY_NODE_ID");
   District_Id      : String := EV.Value (Name => "CITY_DISTRICT_ID");
-- Configuration Path of the project
   Config_Path      : String
      := (EV.Value (Name => "CITY_ROOT")) & "/etc/init/" & City_Id & "/";
-- Names of the configuration files
   Hosts_File       : String := "hosts" & District_Id & ".conf";
   Resources_File   : String := "resources" & District_Id & ".conf";
-- Configuration Objects
   Network_Config   : G_JSON.JSON_Value := G_JSON.Create_Object;
   Resources_Config : G_JSON.JSON_Value := G_JSON.Create_Object;
   AI_Interface     : access AI.Object'Class := new AI.Adapter.Object;
   begin
   -- App config
      Active.Travel.Set_AI (AI_Interface);

      Network_Config :=
         RJ.Parse (RJ.Object (File_Reader), Config_Path & Hosts_File);
      Resources_Config :=
         RJ.Parse (RJ.Object (File_Reader), Config_Path & Resources_File);
   ---------------------
   -- INIT (BOTTOM-UP)
   ---------------------
   -- INIT SESSION LAYER
      Interface_Layer.Session.Init
         (Application_Address =>
            G_JSON.Get (Network_Config, "app_name"),
         Application_Port =>
            G_Socket.Port_Type'Value
               (G_JSON.Write (G_JSON.Get (Network_Config, "app_port"))),
         Middleware_Address =>
            G_JSON.Get (Network_Config, "mw_name"),
         Middleware_Port =>
            G_Socket.Port_Type'Value
               (G_JSON.Write (G_JSON.Get (Network_Config, "mw_port"))),
         Receivers_Pool_Size =>
            Positive'Value
               (G_JSON.Write (G_JSON.Get (Resources_Config, "rx_pool_size")))
         );

      -- INIT PRESENTATION LAYER
      Interface_Layer.Presentation.Init;

      -- INIT SERVICE LAYER
      Interface_Layer.Service.Init
         (Request_Pool_Size =>
            Positive'Value
               (G_JSON.Write
                  (G_JSON.Get (Resources_Config, "activator_req_pool_size"))),
         Ack_Pool_Size =>
            Positive'Value
               (G_JSON.Write
                  (G_JSON.Get (Resources_Config, "activator_ack_pool_size")))
         );

      -- INIT ACTIVE ENTITIES
      Active.System.Init;

      -- INIT REACTIVE ENTITIES
      Reactive.System.Init;

   ---------------------
   -- START (TOP-DOWN)
   -- Application Layer started by Middleware
   ---------------------
      -- START SERVICE LAYER
      Interface_Layer.Service.Start;
      -- START PRESENTATION LAYER
      Interface_Layer.Presentation.Start;
      -- START SESSION LAYER
      Interface_Layer.Session.Start;
Ada.Text_IO.Put_Line ("APPLICATION STARTED");

end Init;
