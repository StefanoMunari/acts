--------------------------------------------------------------------------------
-- @author <stefanomunari.sm@gmail.com>
-- @context application-backend::shared-shared_references
-- @purpose This is a generic package which requires an explicit instantiation
--          of the type T. It manages the lifetime of the 'Reference to T'
--          transparently to the user. Furthermore, it allows to share the same
--          shared_reference (which implies sharing the same 'Reference to T')
--          between more than one shared_reference. It is a thread-safe class,
--          which means it can be used concurrently by several threads. The
--          reponsibility to free the 'Reference to T' is encapsulated inside
--          the class itself. In this sense, no action has to be taken by the
--          user
-- @interface Init (T_Reference):
--              initialize the shared_reference with a instance of
--              'Reference to T'. It triggers a Reference_Counter increment.
--              Warning: a CONSTRAINT_ERROR is raised if T_Reference is NULL
--            Init (Shared_Record_Reference):
--              initialize the shared_reference with the record of another
--              shared_reference. In this situation we expect the other
--              shared_reference to share its record.
--              A common usage pattern is the following:
--                  Shared_Ref0.Init (Shared_Ref1.Share)
--            Share -> Shared_Record_Reference (by reference):
--              returns its record (to be shared with another shared_reference)
--              Warning: a CONSTRAINT_ERROR is raised if the record is not
--                      initialized (i.e. empty)
--            Get -> T (by copy):
--              returns by copy the object (T) pointed by 'Reference to T'
--            Get_Reference -> T_Reference (by reference):
--              returns by reference the 'Reference to T'
-- @dependencies application-backend::shared-atomics
-- @details The following schema represents the memory layout of the class:
-- HEAP:
--      ______       ___________________________________________
--      |  T | <---------('Reference to T') | Reference_Counter|
--      |____|       |______________________|__________________|
--                      ^
-- STACK:               |
--                   ___|____      ____________
--                   | Data | <----| (Wrapper)|
--                   |______|      |__________|
--                    =========================
--                    \  (Shared_Reference)   /
--                    ========================
--------------------------------------------------------------------------------
with Shared.Atomics;
-- core
with Ada.Finalization;

generic
    type T (<>) is tagged private;
package Shared.Shared_References is

    type T_Reference is access all T'Class;
    type Shared_Record is new Ada.Finalization.Limited_Controlled with private;
    -- limited private;
    type Shared_Record_Reference is access all Shared_Record;
    type Shared_Reference_Wrapper is new Ada.Finalization.Controlled with private;

    protected type Shared_Reference is
        procedure Init (Instance_Reference : in T_Reference);
        procedure Init (Shared_Data_Ref : in Shared_Record_Reference);
        procedure Finalize;
        function Share return Shared_Record_Reference;
        function Get return T;
        function Get_Reference return T_Reference;
        -- necessary for testing
        function Get_Counter return Natural;
    private
        function Is_Valid_Data (To_Check : Shared_Record_Reference)
        return Boolean;

        Data    : Shared_Record_Reference;
        Wrapper : Shared_Reference_Wrapper;
    end Shared_Reference;

private
    type Shared_Record is new Ada.Finalization.Limited_Controlled with
    -- limited
    record
        Reference         : T_Reference;
        Reference_Counter : Shared.Atomics.Atomic_Counter;
    end record;

    overriding procedure Initialize (This : in out Shared_Record);
    overriding procedure Finalize (This : in out Shared_Record);

    type Shared_Reference_Wrapper is new Ada.Finalization.Controlled with
    record
        Data : Shared_Record_Reference;
    end record;

    overriding procedure Initialize (This : in out Shared_Reference_Wrapper);
    overriding procedure Adjust (This : in out Shared_Reference_Wrapper);
    overriding procedure Finalize (This : in out Shared_Reference_Wrapper);

   type Shared_Ref_Ptr is access Shared.Shared_References.Shared_Reference;

end Shared.Shared_References;