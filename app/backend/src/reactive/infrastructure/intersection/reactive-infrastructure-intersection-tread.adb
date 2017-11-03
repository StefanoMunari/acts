separate (Reactive.Infrastructure.Intersection)
procedure Tread (
   This         : in out Intersection.Object;
   Traveller_Id : in     Agent.Agent_Id;
   Advanced     :    out Boolean)
is
   Affected_By_Traffic_Lights : Boolean := FALSE;
   Source_Stretch             : Infra_Id
      := This.Traveller_Utils.Get_Position (Traveller_Id);
   Destination_Stretch        : Infra_Id
      := This.Traveller_Utils.Look_Ahead_Step (Traveller_Id, 1);
   Source_Direction           : Shared.Direction.Cardinal;
   Destination_Direction      : Shared.Direction.Cardinal;
   Direction                  : Shared.Direction.Any;
   Direction_Found            : Boolean := FALSE;
   Already_In                 : Boolean;
   Source                     : Shared.Direction.Cardinal;
   Destination                : Shared.Direction.Cardinal;
   Crossed                    : Boolean := FALSE;
   Exited_Intersection        : Boolean := FALSE;
   Next_Street_Id             : Infra_Id;
begin
   Advanced := FALSE;

   for D in Shared.Direction.Cardinal'Range loop
      if This.Stretches_Existence (D) then
         if This.Stretches (D).Contains (Source_Stretch) then
            Source_Direction := D;
         end if;
      end if;
      if This.Stretches_Existence (D) then
         if This.Stretches (D).Contains (Destination_Stretch) then
            Destination_Direction := D;
         end if;
      end if;
   end loop;

   Direction := Shared.Direction.Combine (
      Source_Direction, Destination_Direction);

   This.Traveller_Utils.Is_Affected_By_Semaphores (
      Traveller_Id, Affected_By_Traffic_Lights);
   if not Affected_By_Traffic_Lights then
   -- try to forward the traveller directly to the next street
   -- Consume another step to tread the intersection
      This.Traveller_Utils.Consume_Step (Traveller_Id);
   -- get next street id
      Destination := Shared.Direction.Get_Destination (Direction);
      Next_Street_Id := This.Streets (Destination);
   -- move the traveller straight to the next street without passing for the
   -- crossing
      This.Infrastructure_Utils.Tread (
            Old_Position => Source_Stretch,
            Treadable_Id => Next_Street_Id,
            Traveller_Id => Traveller_Id,
            Advanced     => Advanced);
      return;
   end if;

   Source := Shared.Direction.Get_Source (Direction);
   This.Entries.Try_To_Enter (Traveller_Id, Source, Advanced, Already_In);
   if not Already_In then
      if Advanced then
      -- Defer traveller for next action
         This.Traveller_Utils.Defer (Traveller_Id, FALSE);
      end if;
      return;
   end if;

   if Direction_Found then
      This.Crossing_Strategy.Cross (Traveller_Id, Direction, Crossed);
   else
      raise Constraint_Error with "Traveller direction not calculable";
   end if;
   if Crossed then
   -- If not, do not make it advance
   --    * Reschedule crossing action for attempting during next tick
      Destination := Shared.Direction.Get_Destination (Direction);
      if This.Streets_Existence (Destination) then
         Next_Street_Id := This.Streets (Destination);
         This.Infrastructure_Utils.Tread (
            Old_Position => Source_Stretch,
            Treadable_Id => Next_Street_Id,
            Traveller_Id => Traveller_Id,
            Advanced     => Advanced);
      end if;
   end if;
end Tread;
