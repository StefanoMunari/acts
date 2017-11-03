package Shared.Atomics is

	protected type Atomic_Counter is
		procedure Increment;
		procedure Decrement;
		function Get return Natural;
	private
		Instance : Natural := 0;
	end Atomic_Counter;

end Shared.Atomics;