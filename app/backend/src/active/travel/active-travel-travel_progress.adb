with Active.Agent;
with Active.Build;
with Active.Travel.Travel_Completed;
with Active.Traveller;

package body Active.Travel.Travel_Progress is

   package Agent renames Active.Agent;
   package Build renames Active.Build;

   function Get_Instance (
      Travel_Completed :
        access Active.Travel.Travel_Completed.Object'Class := null;
      Infrastructure_Utils :
        access Reactive.Infrastructure.Utils.Object'Class := null;
      Space_Master :
        access Active.Space_Master.Object'Class := null)
   return Travel_Progress.Reference is
   begin
      if Instance = null then
         Instance := new Travel_Progress.Object;
      end if;

      if (Travel_Completed = null) then
         Instance.Travel_Completed
           := Active.Travel.Travel_Completed.Get_Instance;
      else
         Instance.Travel_Completed := Travel_Completed;
      end if;

      if (Infrastructure_Utils = null) then
         Instance.Infrastructure_Utils
           := Reactive.Infrastructure.Utils.Get_Instance;
      else
         Instance.Infrastructure_Utils := Infrastructure_Utils;
      end if;

      if (Space_Master = null) then
         Instance.Space_Master := Active.Space_Master.Get_Instance;
      else
         Instance.Space_Master := Space_Master;
      end if;

      return Instance;
   end Get_Instance;

   procedure Advance (This   : in out Travel_Progress.Object;
                      Travel : in out Active.Travel.Object'Class)
   is
      Advanced             : Boolean;
      Previous_Step        : Infra_Id := Travel.Get_Previous_Step_Id;
      Current_Step         : Infra_Id := Travel.Get_Current_Step_Id;
      Current_Traveller_Id : Agent.Agent_Id := Travel.Get_Traveller_Id;
   begin
      This.Infrastructure_Utils
         .Tread (Old_Position => Previous_Step,
                 Treadable_Id => Current_Step,
                 Traveller_Id => Current_Traveller_Id,
                 Advanced     => Advanced);

   -- Check if the traveller finished the travel
      if Natural (Travel.Route.Length) = 0 then
         Travel.Change_Travel_State (This.Travel_Completed);
      -- Reschedule with retry?
      --+ In this way Advance of Travel_Completed will be called
         This.Space_Master.Defer (Current_Traveller_Id, True);
      else
         if Advanced then
         -- If Advanced (and travel not finished) schedule without deferring
            This.Space_Master.Defer (Current_Traveller_Id, False);
         end if;
      end if;
   end Advance;

   function Has_Next_Step (This   : in Travel_Progress.Object;
                           Travel : in Active.Travel.Object'Class)
                           return Boolean is
   begin
      return Travel.Get_Current_Step_Id /= Travel.Get_Last_Step_Id;
   end Has_Next_Step;

   function Is_Progressing (This   : in Travel_Progress.Object;
                            Travel : in Active.Travel.Object'Class)
                            return Boolean is
   begin
      return TRUE;
   end Is_Progressing;

   function Dump (This : Travel_Progress.Object)
   return SU.Unbounded_String
   is (SU.To_Unbounded_String (Build.Progress_State));

end Active.Travel.Travel_Progress;
