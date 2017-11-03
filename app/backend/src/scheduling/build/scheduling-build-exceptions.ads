------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::scheduling-build-exceptions
-- @purpose Define procedures to raise exceptions related to the scheduling
--          subsystem build process
-- @interface Raise_Missing_Field_For_Scheduling
--              raise an exception when the JSON provided for the scheduling
--              subsystem is malformed
-- @dependencies -
-- @details -
------------------------------------------------------------------------------

package Scheduling.Build.Exceptions is

   Missing_Field_For_Scheduling : exception;

   procedure Raise_Missing_Field_For_Scheduling (
      Field_Name : in String);

end Scheduling.Build.Exceptions;
