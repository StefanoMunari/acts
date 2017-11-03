------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::reactive
-- @purpose Enclosing package for the reactive sub-system
-- @interface -
-- @dependencies -
-- @details This package contains the definition of two types.
--          * Stretch_Type is used to identify different types of stretches
--            trod by travellers
--          * Infra_Id is the type of reactive objects' identifiers
------------------------------------------------------------------------------

package Reactive is

   package Stretch_Type_Package is
      type Stretch_Type is (FOOT, BIKE, ROAD);
   end Stretch_Type_Package;

   package Infra_Id_Type is
      type Infra_Id is new Natural;
   end Infra_Id_Type;

end Reactive;
