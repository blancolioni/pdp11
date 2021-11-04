with Pdp11.Drivers;

package Pdp11.Object_Driver is

   Max_Properties : constant := 64;

   type Property_Index is range 1 .. Max_Properties;

   type Object_Interface is interface;

   function Name
     (Object : Object_Interface)
      return String
      is abstract;

   function Last_Property
     (Object : Object_Interface)
      return Property_Index
      is abstract;

   function Get_Word_16
     (Object : Object_Interface;
      Index  : Property_Index)
      return Word_16
      is abstract;

   procedure Set_Word_16
     (Object : in out Object_Interface;
      Index  : Property_Index;
      Value  : Word_16)
   is abstract;

   function Create_Object_Driver
     (Object : Object_Interface'Class)
      return Pdp11.Drivers.Pdp11_Driver;

end Pdp11.Object_Driver;
