with Interface_Layer.Utils.Unmarshaller;

package Interface_Layer.Utils.Unmarshaller.Utility is
   package Unmarshaller renames Interface_Layer.Utils.Unmarshaller;
   
   function Get_Unmarshaller (Data : String) return Unmarshaller.Reference;

end Interface_Layer.Utils.Unmarshaller.Utility;
