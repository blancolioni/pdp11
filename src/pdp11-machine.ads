with Pdp11.ISA;

with Pdp11.Drivers;
with Pdp11.Memory;

package Pdp11.Machine is

   Halted           : exception;
   Bus_Error        : exception;
   ISA_Error        : exception;
   Division_By_Zero : exception;

   type Machine_Type is
     new Pdp11.Memory.Root_Memory_Type with private;

   procedure Clear_Drivers
     (Machine : in out Machine_Type'Class);

   procedure Add_Driver
     (Machine : in out Machine_Type'Class;
      Driver  : not null access Pdp11.Drivers.Root_Driver_Type'Class;
      Base    : Address_Type);

   procedure Execute
     (Machine : in out Machine_Type'Class;
      Quantum : ISA.Microsecond_Duration;
      Used    : out ISA.Microsecond_Duration);

   subtype Machine_Register is Pdp11.ISA.Register_Index;

   procedure Set_Register
     (Machine  : in out Machine_Type'Class;
      Register : Machine_Register;
      Value    : Word_16);

   procedure Report (Machine : Machine_Type'Class);

private

   type Register_Array is
     array (Pdp11.ISA.Register_Index) of Word_16;

   type Float_Register_Array is
     array (Pdp11.ISA.FP_Register_Index) of Float_32;

   type Vector_96 is
      record
         X, Y, Z : Float_32;
      end record;

   type Vector_Register_Array is
     array (Pdp11.ISA.V_Register_Index) of Vector_96;

   type Driver_Index is range 0 .. 255;

   type Address_Driver_Map is
     array (Address_Type range 0 .. 1023) of Driver_Index;

   function Get_Page (Address : Address_Type) return Address_Type
   is (Address / 64);

   type Driver_Record is
      record
         Base   : Address_Type            := 0;
         Driver : Pdp11.Drivers.Pdp11_Driver;
      end record;

   type Installed_Driver_Array is
     array (Driver_Index) of Driver_Record;

   type Machine_Type is
     new Pdp11.Memory.Root_Memory_Type with
      record
         Rs                  : Register_Array :=
                                 (0, 1, 2, 3, 4, 5, 6, 7);
         ACs                 : Float_Register_Array := (others => 0.0);
         VRs                 : Vector_Register_Array :=
                                 (others => (others => 0.0));
         Installed_Drivers   : Installed_Driver_Array;
         Driver_Map          : Address_Driver_Map := (others => 0);
         Started             : Boolean := False;
         N, Z, V, C          : Boolean := False;
         Clock               : ISA.Microsecond_Duration := 0.0;
         Current_Instruction : ISA.Instruction_Type;
         Current_Timing      : ISA.Microsecond_Duration;
      end record;

   overriding function Get_Word_16
     (Machine : Machine_Type;
      Address : Address_Type)
      return Word_16;

   overriding function Get_Word_8
     (Machine : Machine_Type;
      Address : Address_Type)
      return Word_8;

   overriding function Get_Float_32
     (Machine : Machine_Type;
      Address : Address_Type)
      return Float_32;

   overriding procedure Set_Word_16
     (Machine : in out Machine_Type;
      Address : Address_Type;
      Value   : Word_16);

   overriding procedure Set_Word_8
     (Machine : in out Machine_Type;
      Address : Address_Type;
      Value   : Word_8);

   overriding procedure Set_Float_32
     (Machine : in out Machine_Type;
      Address : Address_Type;
      Value   : Float_32);

end Pdp11.Machine;
