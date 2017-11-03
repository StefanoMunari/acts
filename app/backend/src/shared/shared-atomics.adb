package body Shared.Atomics is

	protected body Atomic_Counter is
		procedure Increment is
		begin
			Instance := Instance + 1;
		end Increment;
		procedure Decrement is
		begin
			if Instance > 0 then
				Instance := Instance - 1;
			end if;
		end Decrement;
		function Get return Natural is
		begin
			return Instance;
		end Get;
	end Atomic_Counter;

end Shared.Atomics;