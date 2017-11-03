package Interface_Layer.Service.Activators.Requests.Executors is

   task type Executor is
      entry Init (Task_Index : Stacks.Stack_Range);
      entry Exec;
      entry Shutdown;
   end Executor;

end Interface_Layer.Service.Activators.Requests.Executors;
