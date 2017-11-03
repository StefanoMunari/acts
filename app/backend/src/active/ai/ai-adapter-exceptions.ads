------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::ai-adapter-exceptions
-- @purpose Define procedures to raise AI-related exceptions
-- @interface Raise_Init_Exception
--              raise an exception when incurring in some init-time errors
--            Raise_Find_Exception
--              raise an exception if the AI is unable to find a path for the
--              given traveller
--            Raise_Limit_Exception
--              raise an exception when incurring in an error while setting
--              the clients limit
--            Raise_Finalize_Exception
--              raise an exception when finalizing the AI service
-- @dependencies -
-- @details -
------------------------------------------------------------------------------

package AI.Adapter.Exceptions is

   procedure Raise_Init_Exception;

   procedure Raise_Find_Exception;

   procedure Raise_Limit_Exception;

   procedure Raise_Finalize_Exception;

end AI.Adapter.Exceptions;
