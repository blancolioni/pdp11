package body Pdp11.Devices.RAM is

   subtype Parent is Pdp11.Devices.Instance;

   type Word_8_Array is array (Address_Type range <>) of Word_8;

   type Instance (Last : Address_Type) is new Parent with
      record
         M : Word_8_Array (0 .. Last);
      end record;

   overriding function Name (This : Instance) return String
   is ("RAM");

   overriding procedure Tick
     (This    : in out Instance;
      Elapsed : ISA.Microsecond_Duration;
      Handler : not null access Interrupt_Handler'Class)
   is null;

   overriding procedure Get_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_8);

   overriding procedure Set_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_8);

   ------------
   -- Create --
   ------------

   function Create
     (Base, Bound : Address_Type)
      return Reference
   is
      Last : constant Address_Type := Bound - Base;
   begin
      return new Instance'
        (Pdp11.Devices.Parent with
           Priority => <>,
           Vector   => <>,
           Base     => Base,
           Bound    => Bound,
           Last     => Last,
           M    => (others => 0));
   end Create;

   ----------------
   -- Get_Word_8 --
   ----------------

   overriding procedure Get_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_8)
   is
   begin
      Value := This.M (Address);
   end Get_Word_8;

   ----------------
   -- Set_Word_8 --
   ----------------

   overriding procedure Set_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_8)
   is
   begin
      This.M (Address) := Value;
   end Set_Word_8;

end Pdp11.Devices.RAM;
