--------------------------------------------------------------------------------
-- @author <stefanomunari.sm@gmail.com>
-- @context application-backend::shared-shared_references
-- @outline Wrapper:
--          It is a necessary data field. It enables to correctly deallocate
--          and pass the Shared_Reference.Data data field. Indeed, a
--          protected object (shared_reference) can not provide by itself
--          automatic finalization (not possible to implement
--          Finalization.Limited_Controlled). So, the responsibility of
--          Shared_Reference.Wrapper is to control the Data deallocation.
--          Thus providing to Shared_Reference the Finalization
--          functionality. Note that the allocation responsibility is
--          assigned to Shared_Reference itself.
--          Hint: think Shared_Reference and Shared_Refernece.Wrapper as
--             complementary classes
-- @check Init (T_Reference)
-- @check Finalize (Shared_Reference_Wrapper)
-- @check Init (Shared_Record_Reference)
--------------------------------------------------------------------------------
-- core
with Ada.Unchecked_Deallocation;
-- with Ada.Text_IO;use Ada.Text_IO;

package body Shared.Shared_References is

   protected body Shared_Reference is

   procedure Init (Instance_Reference : in T_Reference) is
   begin
      if Instance_Reference = NULL
      then
         raise CONSTRAINT_ERROR;
      end if;
      Wrapper.Finalize;
      Data := new Shared_Record;
      Data.all.Reference := Instance_Reference;
      Data.all.Reference_Counter.Increment;
      Wrapper.Data := Data;
   end Init;

   procedure Init (Shared_Data_Ref : in Shared_Record_Reference) is
   begin
   -- handle (possible) Data finalization
      Wrapper.Finalize;
   -- make the two shared_reference to point the same data record (share it)
      Data := Shared_Data_Ref;
   -- update wrapper (has no ownership on the data record)
   -- update Wrapper_
      Wrapper.Data := Data;
      Data.all.Reference_Counter.Increment;
   end Init;

   procedure Finalize is
   begin
      Wrapper.Finalize;
   end Finalize;

   function Share return Shared_Record_Reference is
   begin
      if not Is_Valid_Data (Wrapper.Data)
      then
         raise CONSTRAINT_ERROR;
      end if;
      return Wrapper.Data;
   end Share;

   function Get return T is
   begin
      if not Is_Valid_Data (Data)
      then
         raise CONSTRAINT_ERROR;
      end if;
      return T (Data.all.Reference.all);
   end Get;

   function Get_Reference return T_Reference is
   begin
      if not Is_Valid_Data (Data)
      then
         raise CONSTRAINT_ERROR;
      end if;
      return Data.all.Reference;
   end Get_Reference;

   -- necessary for testing
   function Get_Counter return Natural is
   begin
      if Data = NULL then
         return 0;
      end if;
      return Data.all.Reference_Counter.Get;
   end Get_Counter;

   -- privte
   function Is_Valid_Data (To_Check : Shared_Record_Reference) return Boolean
   is begin
      if To_Check = NULL
      then
         raise PROGRAM_ERROR;
         return False;
      end if;
      if To_Check.all.Reference = NULL
      then
         raise CONSTRAINT_ERROR;
         return False;
      end if;
      return True;
   end Is_Valid_Data;

   end Shared_Reference;

   overriding
   procedure Initialize (This : in out Shared_Record) is
   begin
      null;
   end Initialize;
   overriding
   procedure Finalize (This : in out Shared_Record) is
   begin
      null;
   end Finalize;

   overriding
   procedure Initialize (This : in out Shared_Reference_Wrapper) is separate;
   overriding
   procedure Adjust (This : in out Shared_Reference_Wrapper) is separate;
   overriding
   procedure Finalize (This : in out Shared_Reference_Wrapper) is separate;

end Shared.Shared_References;