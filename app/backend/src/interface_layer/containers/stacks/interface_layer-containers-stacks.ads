-- Use stack to pop the next free task index. When a task finishes its
-- asynchronous (no rendezvous) phase, it pushes the index back on the stack.
package Interface_Layer.Containers.Stacks is

   -- class constant
   Maximum_Size : constant := 20;

   -- define types used by Stack
   type Integer_List is array (1..Maximum_Size) of Integer;
   subtype Counter is Integer range 0 .. Maximum_Size;
   subtype Stack_Range is Integer range 1 .. Maximum_Size;

   protected type Stack is
      procedure Init (Size : Stack_Range);
      function Is_Empty return Boolean;
      function Get_Size return Stack_Range;
      procedure Push (Element_Index : in Stack_Range);
      entry Pop (Element_Index : out Stack_Range);
   private
      -- actual Stack Size
      Stack_Size : Stack_Range;
      -- Stack Elements
      Elements : Integer_List;
      -- Stack Pointer points at the current used Stack Element
      Stack_Pointer: Counter := 0;
   end Stack;

end Interface_Layer.Containers.Stacks;