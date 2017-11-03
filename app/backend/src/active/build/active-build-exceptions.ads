------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-build-exceptions
-- @purpose Define procedures to raise exceptions related to the active
--          subsystem build process
-- @interface Raise_Missing_Field_For_District
--              raise an exception when a high-level information is missing
--            Raise_Missing_Field_For_Traveller
--              raise an exception when traveller-related information is
--              missing
--            Raise_Missing_Field_For_Slice
--              raise an exception when a slice is incomplete
--            Raise_Missing_Field_For_Traffic_Light
--              raise an exception when traffic light-related information is
--              incomplete
--            Raise_Invalid_Field_For_Traveller
--              raise an exception when there is an invalid value in a
--              traveller JSON
--            Raise_Invalid_Field_For_Traffic_Light
--              raise an exception when there is an invalid value in a
--              traffic light JSON
-- @dependencies -
-- @details -
------------------------------------------------------------------------------

package Active.Build.Exceptions is

   Missing_Field_For_District      : exception;
   Missing_Field_For_Traffic_Light : exception;
   Missing_Field_For_Traveller     : exception;
   Missing_Field_For_Slice         : exception;
   Invalid_Field_For_Traffic_Light : exception;
   Invalid_Field_For_Traveller     : exception;

   procedure Raise_Missing_Field_For_District (
      Field_Name : in String);

   procedure Raise_Missing_Field_For_Traffic_Light (
      Field_Name : in String);

   procedure Raise_Missing_Field_For_Traveller (
      Field_Name : in String);

   procedure Raise_Missing_Field_For_Slice (
      Field_Name : in String);

   procedure Raise_Invalid_Field_For_Traffic_Light (
      Field_Name : in String);

   procedure Raise_Invalid_Field_For_Traveller (
      Field_Name : in String);

end Active.Build.Exceptions;
