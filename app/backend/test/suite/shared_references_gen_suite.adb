package body Shared_References_Gen_Suite is

   function Suite return Access_Test_Suite is
      TS : constant Access_Test_Suite := new Test_Suite;
   begin
      TS.Add_Test (Caller.Create
         (Name => Instance_Name & "Test_Init_Reference_Success",
          Test => Test_Init_Reference_Success'Access));
      TS.Add_Test (Caller.Create
         (Name => Instance_Name & "Test_Init_Reference_Fail",
          Test => Test_Init_Reference_Fail'Access));
      TS.Add_Test (Caller.Create
         (Name => Instance_Name & "Test_Init_Shared_Record_Success",
          Test => Test_Init_Shared_Record_Success'Access));
      TS.Add_Test (Caller.Create
         (Name => Instance_Name & "Test_Init_Shared_Record_Fail",
          Test => Test_Init_Shared_Record_Fail'Access));
      TS.Add_Test (Caller.Create (Name => Instance_Name & "Test_Share_Success",
                       Test => Test_Share_Success'Access));
      TS.Add_Test (Caller.Create (Name => Instance_Name & "Test_Share_Fail",
                       Test => Test_Share_Fail'Access));
      TS.Add_Test (Caller.Create (Name => Instance_Name & "Test_Get_Success",
                       Test => Test_Get_Success'Access));
      TS.Add_Test (Caller.Create (Name => Instance_Name & "Test_Get_Fail",
                       Test => Test_Get_Fail'Access));
      TS.Add_Test (Caller.Create (Name => Instance_Name & "Test_Get_Reference_Success",
                       Test => Test_Get_Reference_Success'Access));
      TS.Add_Test (Caller.Create (Name => Instance_Name & "Test_Get_Reference_Fail",
                       Test => Test_Get_Reference_Fail'Access));
      TS.Add_Test (Caller.Create (Name => Instance_Name & "Test_Get_Counter_Zero",
                       Test => Test_Get_Counter_Zero'Access));
      TS.Add_Test (Caller.Create (Name => Instance_Name & "Test_Get_Counter",
                       Test => Test_Get_Counter'Access));
      TS.Add_Test (Caller.Create (Name => Instance_Name & "Test_Reference_Counting_3",
                       Test => Test_Reference_Counting_3'Access));
      TS.Add_Test (Caller.Create (Name => Instance_Name & "Test_Concurrent_Share",
                       Test => Test_Concurrent_Share'Access));
      return TS;
   end Suite;
end Shared_References_Gen_Suite;
