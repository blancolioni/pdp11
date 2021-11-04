with Ada.Containers.Indefinite_Holders;

package body Pdp11.Object_Driver is

   package Object_Holders is
     new Ada.Containers.Indefinite_Holders (Object_Interface'Class);

   type Object_Driver_Type is
     new Pdp11.Drivers.Root_Driver_Type with
      record
         Object : Object_Holders.Holder;
      end record;

   overriding function Name
     (Driver : Object_Driver_Type)
      return String
   is (Driver.Object.Element.Name);

   overriding function Bound
     (Driver : Object_Driver_Type)
      return Pdp11.Address_Type
   is (if Driver.Object.Is_Empty
       then 0
       else Address_Type (Driver.Object.Element.Last_Property * 2));

   overriding function Get_Word_16
     (Driver  : Object_Driver_Type;
      Address : Address_Type)
      return Word_16;

   overriding procedure Set_Word_16
     (Driver  : in out Object_Driver_Type;
      Address : Address_Type;
      Value   : Word_16);

   --------------------------
   -- Create_Object_Driver --
   --------------------------

   function Create_Object_Driver
     (Object : Object_Interface'Class)
      return Pdp11.Drivers.Pdp11_Driver
   is
   begin
      return new Object_Driver_Type'
        (Pdp11.Drivers.Root_Driver_Type with
           Object => Object_Holders.To_Holder (Object));
   end Create_Object_Driver;

   -----------------
   -- Get_Word_16 --
   -----------------

   overriding function Get_Word_16
     (Driver  : Object_Driver_Type;
      Address : Address_Type)
      return Word_16
   is
   begin
      return Driver.Object.Element.Get_Word_16
        (Property_Index (Address / 2 + 1));
   end Get_Word_16;

   -----------------
   -- Set_Word_16 --
   -----------------

   overriding procedure Set_Word_16
     (Driver  : in out Object_Driver_Type;
      Address : Address_Type;
      Value   : Word_16)
   is
   begin
      Driver.Object.Reference.Set_Word_16
        (Property_Index (Address / 2 + 1), Value);
   end Set_Word_16;

end Pdp11.Object_Driver;
