package body Reactive.Infrastructure.Build.Exceptions is

   procedure Raise_Missing_Field_For_Stretch (Field_Name : in String) is
   begin
      raise Missing_Field_For_Stretch
        with "Missing set for field '" & Field_Name & "' of stretch";
   end Raise_Missing_Field_For_Stretch;

   procedure Raise_Missing_Field_For_Lane (Field_Name : in String) is
   begin
      raise Missing_Field_For_Lane
        with "Missing set for field '" & Field_Name & "' of lane";
   end Raise_Missing_Field_For_Lane;

   procedure Raise_Missing_Field_For_Way (Field_Name : in String) is
   begin
      raise Missing_Field_For_Way
        with "Missing set for field '" & Field_Name & "' of way";
   end Raise_Missing_Field_For_Way;

   procedure Raise_Missing_Field_For_Street (Field_Name : in String) is
   begin
      raise Missing_Field_For_Street
        with "Missing set for field '" & Field_Name & "' of street";
   end Raise_Missing_Field_For_Street;

   procedure Raise_Missing_Field_For_District (Field_Name : in String) is
   begin
      raise Missing_Field_For_District
        with "Missing set for field '" & Field_Name & "' of district";
   end Raise_Missing_Field_For_District;

   procedure Raise_Missing_Field_For_Intersection (
    Field_Name : in String) is
   begin
      raise Missing_Field_For_Intersection
        with "Missing set for field '" & Field_Name & "' of intersection";
   end Raise_Missing_Field_For_Intersection;

   procedure Raise_Missing_Field_For_Host (
    Field_Name : in String) is
   begin
      raise Missing_Field_For_Host
        with "Missing set for field '" & Field_Name & "' of Host";
   end Raise_Missing_Field_For_Host;

   procedure Raise_Invalid_Field_For_Intersection (
    Field_Name : in String) is
   begin
      raise Invalid_Field_For_Intersection
        with "Invalid set for field '" & Field_Name & "' of intersection";
   end Raise_Invalid_Field_For_Intersection;

end Reactive.Infrastructure.Build.Exceptions;
