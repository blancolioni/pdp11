with Ada.Containers.Indefinite_Holders;
with Ada.Directories;
with Ada.Strings.Unbounded;

with WL.Binary_IO;

package body Pdp11.Drivers.ROM is

   type ROM_Array is array (Address_Type range <>) of Word_8;

   package ROM_Holders is
     new Ada.Containers.Indefinite_Holders (ROM_Array);

   type ROM_Driver is
     new Root_Driver_Type with
      record
         Memory : ROM_Holders.Holder;
         Name   : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Bound
     (Driver : ROM_Driver)
      return Address_Type
   is (Driver.Memory.Element'Last + 1);

   overriding function Name
     (Driver : ROM_Driver)
      return String
   is (Ada.Strings.Unbounded.To_String (Driver.Name));

   overriding function Get_Word_8
     (Driver : ROM_Driver;
      Address : Address_Type)
      return Word_8
   is (Driver.Memory.Element (Address));

   overriding procedure Set_Word_8
     (Driver  : in out ROM_Driver;
      Address : Address_Type;
      Value   : Word_8);

   -----------------------
   -- Create_ROM_Driver --
   -----------------------

   function Create_ROM_Driver
     (Object_Path : String) return Pdp11.Drivers.Pdp11_Driver
   is
      use WL.Binary_IO;
      File : File_Type;
      Driver : ROM_Driver;
   begin

      Driver.Name :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (Ada.Directories.Simple_Name (Object_Path));

      Open (File, In_File, Object_Path);

      declare
         Memory : ROM_Array (0 .. Address_Type (Length (File)) - 1);
      begin
         for B of Memory loop
            declare
               W : WL.Binary_IO.Word_8;
            begin
               Read (File, W);
               B := Pdp11.Word_8 (W);
            end;
         end loop;

         Driver.Memory := ROM_Holders.To_Holder (Memory);
         Close (File);

         return new ROM_Driver'(Driver);
      end;

   end Create_ROM_Driver;

   ----------------
   -- Set_Word_8 --
   ----------------

   overriding procedure Set_Word_8
     (Driver  : in out ROM_Driver;
      Address : Address_Type;
      Value   : Word_8)
   is
   begin
      Driver.Memory.Reference.Element (Address) := Value;
   end Set_Word_8;

end Pdp11.Drivers.ROM;
