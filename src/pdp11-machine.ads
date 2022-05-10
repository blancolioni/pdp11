with Pdp11.ISA;

with Pdp11.Drivers;
with Pdp11.Addressable;

package Pdp11.Machine is

   Halted           : exception;
   ISA_Error        : exception;
   Division_By_Zero : exception;

   type Machine_Type is
     new Pdp11.Addressable.Root_Addressable_Type
     and Pdp11.Drivers.Interrupt_Handler
   with private;

   type Machine_Reference is access all Machine_Type'Class;

   procedure Create
     (This : in out Machine_Type'Class;
      Memory : not null access Pdp11.Addressable.Root_Addressable_Type'Class);

   procedure Execute
     (Machine : in out Machine_Type'Class;
      Quantum : ISA.Microsecond_Duration;
      Used    : out ISA.Microsecond_Duration);

   subtype Machine_Register is Pdp11.ISA.Register_Index;

   function Get_Register
     (Machine  : Machine_Type'Class;
      Register : Machine_Register)
      return Word_16;

   procedure Set_Register
     (Machine  : in out Machine_Type'Class;
      Register : Machine_Register;
      Value    : Word_16);

   overriding procedure Interrupt
     (Machine  : in out Machine_Type;
      Priority : Interrupt_Priority_Type;
      Vector   : Address_Type);

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

   type Machine_Type is
     new Pdp11.Addressable.Root_Addressable_Type
     and Pdp11.Drivers.Interrupt_Handler with
      record
         Rs                  : Register_Array :=
                                 (0, 1, 2, 3, 4, 5, 6, 7);
         ACs                 : Float_Register_Array := (others => 0.0);
         VRs                 : Vector_Register_Array :=
                                 (others => (others => 0.0));
         Started             : Boolean := False;
         Priority            : Priority_Type := 7;
         T, N, Z, V, C       : Boolean := False;
         Clock               : ISA.Microsecond_Duration := 0.0;
         Current_Instruction : ISA.Instruction_Type;
         Current_Timing      : ISA.Microsecond_Duration;
         Memory              : Pdp11.Addressable.Addressable_Reference;
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

   procedure Set_PS
     (Machine : in out Machine_Type'Class;
      Value   : Word_16);

   function Get_PS
     (Machine : Machine_Type'Class)
      return Word_16;

   function Get_Register
     (Machine  : Machine_Type'Class;
      Register : Machine_Register)
      return Word_16
   is (Machine.Rs (Register));

end Pdp11.Machine;
