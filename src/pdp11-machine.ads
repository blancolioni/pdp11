private with Ada.Calendar;
private with Ada.Containers.Doubly_Linked_Lists;

with Pdp11.ISA;

with Pdp11.Devices;
with Pdp11.Addressable.Memory;

package Pdp11.Machine is

   Halted           : exception;
   ISA_Error        : exception;
   Division_By_Zero : exception;

   subtype Parent is Pdp11.Addressable.Root_Addressable_Type;

   type Instance is
     new Parent
     and Pdp11.Devices.Interrupt_Handler
     and Pdp11.Devices.Device_Manager
   with private;

   type Reference is access all Instance'Class;

   procedure Create
     (This : in out Instance'Class;
      Memory : not null access
        Pdp11.Addressable.Memory.Root_Memory_Type'Class);

   procedure Execute_Next_Instruction
     (This : in out Instance'Class);

   procedure Execute_Quantum
     (This    : in out Instance'Class;
      Quantum : Pdp11.ISA.Microsecond_Duration);

   procedure Execute_Quantum
     (This : in out Instance'Class;
      Quantum : ISA.Microsecond_Duration;
      Used    : out ISA.Microsecond_Duration);

   procedure Start
     (This : in out Instance'Class);

   function Clock
     (This : Instance'Class)
      return ISA.Microsecond_Duration;

   subtype Machine_Register is Pdp11.ISA.Register_Index;

   function Get_Register
     (This     : Instance'Class;
      Register : Machine_Register)
      return Word_16;

   procedure Set_Register
     (This     : in out Instance'Class;
      Register : Machine_Register;
      Value    : Word_16);

   subtype FP_Register is Pdp11.ISA.FP_Register_Index;

   function Get_Float_Register
     (This     : Instance'Class;
      Register : FP_Register)
      return Float_32;

   procedure Set_Float_Register
     (This     : in out Instance'Class;
      Register : FP_Register;
      Value    : Float_32);

   overriding procedure Interrupt
     (This  : in out Instance;
      Priority : Interrupt_Priority_Type;
      Vector   : Address_Type);

   procedure Report (This : Instance'Class);

   procedure Start_Trace;
   procedure Stop_Trace;

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

   package Device_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Pdp11.Devices.Reference, Pdp11.Devices."=");

   type Instance is
     new Parent
     and Pdp11.Devices.Interrupt_Handler
     and Pdp11.Devices.Device_Manager with
      record
         Rs                  : Register_Array :=
                                 (0, 1, 2, 3, 4, 5, 6, 7);
         ACs                 : Float_Register_Array := (others => 0.0);
         VRs                 : Vector_Register_Array :=
                                 (others => (others => 0.0));
         Devices             : Device_Lists.List;
         Started             : Boolean := False;
         Waiting             : Boolean := False;
         Priority            : Priority_Type := 7;
         T, N, Z, V, C       : Boolean := False;
         Clock               : ISA.Microsecond_Duration := 0.0;
         Current_Instruction : ISA.Instruction_Type;
         Current_Timing      : ISA.Microsecond_Duration;
         Start_Time          : Ada.Calendar.Time;
         Start_Clock         : ISA.Microsecond_Duration;
         Memory              : Pdp11.Addressable.Memory.Memory_Reference;
      end record;

   overriding procedure Clear_Devices
     (This : in out Instance);

   overriding procedure Add_Device
     (This     : in out Instance;
      Device   : not null access Pdp11.Devices.Instance'Class);

   overriding procedure Get_Word_16
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_16);

   overriding procedure Get_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_8);

   overriding procedure Get_Float_32
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Float_32);

   overriding procedure Set_Word_16
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_16);

   overriding procedure Set_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_8);

   overriding procedure Set_Float_32
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Float_32);

   procedure Set_PS
     (This    : in out Instance'Class;
      Value   : Word_16);

   function Get_PS
     (This    : Instance'Class)
      return Word_16;

   function Get_Register
     (This : Instance'Class;
      Register : Machine_Register)
      return Word_16
   is (This.Rs (Register));

   function Get_Float_Register
     (This     : Instance'Class;
      Register : FP_Register)
      return Float_32
   is (This.ACs (Register));

   function Clock
     (This : Instance'Class)
      return ISA.Microsecond_Duration
   is (This.Clock);

end Pdp11.Machine;
