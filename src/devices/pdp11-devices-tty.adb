with Ada.Text_IO;

package body Pdp11.Devices.TTY is

   subtype Parent is Pdp11.Devices.Instance;

   subtype TTY_Register_Index is Address_Type range 0 .. 7;

   Characters_Per_Second  : constant := 10.0;
   Character_Output_Delay : constant := 1.0e6 / Characters_Per_Second;

   Reader_Busy_Bit   : constant := 11;
   Reader_Done_Bit   : constant := 7;
   Reader_Intr_Bit   : constant := 6;
   Reader_Enable_Bit : constant := 0;

   Writer_Ready_Bit  : constant := 7;
   Writer_Intr_Bit   : constant := 6;
   Writer_Maint_Bit  : constant := 2;

   subtype Word_8_Bit is Natural range 0 .. 7;

   function Flag
     (Value : Boolean;
      Bit   : Word_8_Bit)
      return Word_8
   is (if Value then 2 ** Bit else 0);

   type Instance is new Parent with
      record
         Reader_Busy   : Boolean := False;
         Reader_Done   : Boolean := False;
         Reader_Intr   : Boolean := False;
         Reader_Enable : Boolean := False;
         Reader_Buffer : Word_8  := 0;
         Writer_Ready  : Boolean := True;
         Writer_Intr   : Boolean := False;
         Writer_Maint  : Boolean := False;
         Writer_Buffer : Word_8  := 0;
         Writer_Wait   : ISA.Microsecond_Duration := 0.0;
      end record;

   overriding function Name (This : Instance) return String
   is ("TTY");

   overriding procedure Tick
     (This    : in out Instance;
      Elapsed : ISA.Microsecond_Duration;
      Handler : not null access Interrupt_Handler'Class);

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
      return Reference
   is
   begin
      return new Instance'
        (Pdp11.Devices.Parent with
           Priority => 4,
           Vector   => 8#040#,
           Base     => 16#FF70#,
           Bound    => 16#FF77#,
           others => <>);
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
      case TTY_Register_Index (Address) is
         when 0 =>
            Value := Flag (This.Reader_Done, Reader_Done_Bit)
              + Flag (This.Reader_Intr, Reader_Intr_Bit)
              + Flag (This.Reader_Enable, Reader_Enable_Bit);
         when 1 =>
            Value := Flag (This.Reader_Busy, Reader_Busy_Bit - 8);
         when 2 =>
            Value := This.Reader_Buffer;
         when 3 =>
            Value := 0;
         when 4 =>
            Value := Flag (This.Writer_Ready, Writer_Ready_Bit)
              + Flag (This.Writer_Intr, Writer_Intr_Bit)
              + Flag (This.Writer_Maint, Writer_Maint_Bit);
         when 5 =>
            Value := 0;
         when 6 =>
            Value := 0;
         when 7 =>
            Value := 0;
      end case;
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
      case TTY_Register_Index (Address) is
         when 0 =>
            This.Reader_Intr := (Value and (2 ** Reader_Intr_Bit)) /= 0;
            This.Reader_Enable := (Value and (2 ** Reader_Enable_Bit)) /= 0;
         when 1 =>
            null;
         when 2 =>
            This.Reader_Buffer := Value;
         when 3 =>
            null;
         when 4 =>
            This.Writer_Intr := (Value and (2 ** Writer_Intr_Bit)) /= 0;
            This.Writer_Maint := (Value and (2 ** Writer_Maint_Bit)) /= 0;
         when 5 =>
            null;
         when 6 =>
            if This.Writer_Ready then
               This.Writer_Buffer := Value;
               This.Writer_Ready := False;
               This.Writer_Wait := Character_Output_Delay;
            end if;
         when 7 =>
            null;
      end case;
   end Set_Word_8;

   ----------
   -- Tick --
   ----------

   overriding procedure Tick
     (This    : in out Instance;
      Elapsed : ISA.Microsecond_Duration;
      Handler : not null access Interrupt_Handler'Class)
   is
      use type ISA.Microsecond_Duration;
   begin
      if This.Writer_Wait > 0.0 then
         if This.Writer_Wait <= Elapsed then
            This.Writer_Wait := 0.0;
            Ada.Text_IO.Put (Character'Val (This.Writer_Buffer));
            Ada.Text_IO.Flush;
            This.Writer_Ready := True;
         else
            This.Writer_Wait := This.Writer_Wait - Elapsed;
         end if;
      end if;
   end Tick;

end Pdp11.Devices.TTY;
