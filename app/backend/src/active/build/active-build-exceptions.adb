package body Active.Build.Exceptions is

   procedure Raise_Missing_Field_For_District (Field_Name : in String) is
   begin
      raise Missing_Field_For_District
        with "Missing set for field '" & Field_Name & "' of district";
   end Raise_Missing_Field_For_District;

   procedure Raise_Missing_Field_For_Traffic_Light (
      Field_Name : in String) is
   begin
      raise Missing_Field_For_Traffic_Light
        with "Missing set for field '" & Field_Name & "' of traffic light";
   end Raise_Missing_Field_For_Traffic_Light;

   procedure Raise_Missing_Field_For_Traveller (
      Field_Name : in String) is
   begin
      raise Missing_Field_For_Traveller
        with "Missing set for field '" & Field_Name & "' of traveller";
   end Raise_Missing_Field_For_Traveller;

   procedure Raise_Missing_Field_For_Slice (
      Field_Name : in String) is
   begin
      raise Missing_Field_For_Slice
        with "Missing set for field '" & Field_Name & "' of slice";
   end Raise_Missing_Field_For_Slice;

   procedure Raise_Invalid_Field_For_Traffic_Light (
      Field_Name : in String) is
   begin
      raise Invalid_Field_For_Traffic_Light
        with "Invalid set for field '" & Field_Name & "' of traffic light";
   end Raise_Invalid_Field_For_Traffic_Light;

   procedure Raise_Invalid_Field_For_Traveller (
      Field_Name : in String) is
   begin
      raise Invalid_Field_For_Traveller
        with "Invalid set for field '" & Field_Name & "' of traveller";
   end Raise_Invalid_Field_For_Traveller;

end Active.Build.Exceptions;
