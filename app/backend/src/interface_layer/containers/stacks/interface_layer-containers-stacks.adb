package body Interface_Layer.Containers.Stacks is

   protected body Stack is

      procedure Init (Size : Stack_Range) is
      begin
         Stack_Size := Size;
         for Element_Index in Stack_Range range 1 .. Size loop
            Push (Element_Index);
         end loop;
      end;

      function Is_Empty return Boolean is
      begin
         return (Stack_Pointer = 0);
      end Is_Empty;

      function Get_Size return Stack_Range is
      begin
         return Stack_Size;
      end Get_Size;

      procedure Push (Element_Index : in Stack_Range) is
      begin -- guarded against overflow (the type Stack_Range is 1 <= x <= Maximum_Size)
         Stack_Pointer := Stack_Pointer + 1;
         Elements (Stack_Pointer) := Element_Index;
      end;

      entry Pop (Element_Index : out Stack_Range) when Stack_Pointer /= 0 is
      begin -- guarded against underflow
         Element_Index := Elements (Stack_Pointer);
         Stack_Pointer := Stack_Pointer - 1;
      end;

   end Stack;

end Interface_Layer.Containers.Stacks;