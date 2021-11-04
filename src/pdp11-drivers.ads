with Pdp11.Memory;

package Pdp11.Drivers is

   type Root_Driver_Type is
     abstract new Pdp11.Memory.Root_Memory_Type with private;

   type Pdp11_Driver is access all Root_Driver_Type'Class;

   function Name
     (Driver : Root_Driver_Type)
      return String
      is abstract;

   function Bound
     (Driver : Root_Driver_Type)
      return Address_Type
      is abstract;

private

   type Root_Driver_Type is
     abstract new Pdp11.Memory.Root_Memory_Type with
      record
         null;
      end record;

end Pdp11.Drivers;
