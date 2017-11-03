package Reactive.Infrastructure.Build.Exceptions is

   Missing_Field_For_Stretch      : exception;
   Missing_Field_For_Lane         : exception;
   Missing_Field_For_Way          : exception;
   Missing_Field_For_Street       : exception;
   Missing_Field_For_District     : exception;
   Missing_Field_For_Intersection : exception;
   Missing_Field_For_Host         : exception;
   Invalid_Field_For_Intersection : exception;

   procedure Raise_Missing_Field_For_Stretch      (
      Field_Name : in String);
   procedure Raise_Missing_Field_For_Lane         (
      Field_Name : in String);
   procedure Raise_Missing_Field_For_Way          (
      Field_Name : in String);
   procedure Raise_Missing_Field_For_Street       (
      Field_Name : in String);
   procedure Raise_Missing_Field_For_District     (
      Field_Name : in String);
   procedure Raise_Missing_Field_For_Intersection (
      Field_Name : in String);
   procedure Raise_Missing_Field_For_Host         (
      Field_Name : in String);
   procedure Raise_Invalid_Field_For_Intersection (
      Field_Name : in String);

end Reactive.Infrastructure.Build.Exceptions;
