package body Pdp11.Devices.RC11 is

   Base_Address       : constant := 8#177440#;
   Bound_Address      : constant := 8#177457#;

   subtype Parent is Pdp11.Devices.Instance;

   subtype Register_Address is Address_Type range 8#000# .. 8#057#;
   type Register_Array is array (Register_Address) of Word_8;

   type Instance is new Parent with
      record
         Rs           : Register_Array := (others => 0);
         Waiting      : Boolean := False;
         Time_To_Intr : Pdp11.ISA.Microsecond_Duration := 0.0;
         Elapsed      : Pdp11.ISA.Microsecond_Duration := 0.0;
      end record;

   overriding function Name (This : Instance) return String
   is ("rc11");

   overriding procedure Tick
     (This    : in out Instance;
      Elapsed : ISA.Microsecond_Duration;
      Handler : not null access Interrupt_Handler'Class)
   is null;

   overriding procedure Get_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_8)
   is null;

   overriding procedure Set_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_8)
   is null;

   ------------
   -- Create --
   ------------

   function Create return Reference is
   begin
      return new Instance'
        (Pdp11.Devices.Parent with
           Priority => 5,
           Vector   => 8#210#,
           Base     => Base_Address,
           Bound    => Bound_Address,
           others => <>);
   end Create;

end Pdp11.Devices.RC11;
