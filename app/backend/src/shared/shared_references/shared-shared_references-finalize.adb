separate (Shared.Shared_References)
overriding
procedure Finalize (This : in out Shared_Reference_Wrapper) is
	procedure Free is new Ada.Unchecked_Deallocation (T'Class, T_Reference);
	procedure Free is new Ada.Unchecked_Deallocation
		(Shared_Record, Shared_Record_Reference);
begin
	if not (This.Data = NULL) then
		This.Data.all.Reference_Counter.Decrement;
		if This.Data.all.Reference_Counter.Get = 0 and not (This.Data.all.Reference = NULL)
		then
			Free (This.Data.all.Reference);

			Free (This.Data);
		end if;
	end if;
end Finalize;