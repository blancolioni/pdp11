with Ada.Exceptions;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;

with Pdp11.Conversions;
with Pdp11.Images;                     use Pdp11.Images;
with Pdp11.Options;

package body Pdp11.Machine is

   Trace_Execution : Boolean := False;
   Limit_Speed     : Boolean := False;

   package Float_32_Functions is
     new Ada.Numerics.Generic_Elementary_Functions (Float_32);

   function Sqrt (X : Float_32) return Float_32
                  renames Float_32_Functions.Sqrt;

   function Flags_Image (This : Instance'Class) return String;
   function Clock_Image (This : Instance'Class) return String;

   procedure Next (This : in out Instance'Class);

   function Get_Operand_Value
     (This    : in out Instance'Class;
      Operand : Pdp11.ISA.Operand_Type;
      Word    : Boolean)
      return Word_16;

   function Get_Operand_Address
     (This    : in out Instance'Class;
      Operand : Pdp11.ISA.Operand_Type;
      Size    : Positive)
      return Address_Type;

   function Get_Operand_Address
     (This    : in out Instance'Class;
      Operand : Pdp11.ISA.Operand_Type;
      Word    : Boolean)
      return Address_Type
   is (Get_Operand_Address
       (This, Operand,
          (if Word then 2 else 1)));

   function Get_Float_Operand_Value
     (This    : in out Instance'Class;
      Operand : Pdp11.ISA.Operand_Type)
      return Float_32;

   procedure Set_Float_Operand_Value
     (This    : in out Instance'Class;
      Operand : Pdp11.ISA.Operand_Type;
      Value   : Float_32);

   procedure Update_Float_Operand_Value
     (This    : in out Instance'Class;
      Operand : Pdp11.ISA.Operand_Type;
      Update  : not null access
        procedure (X : in out Float_32));

   function Get_Vector_Operand_Value
     (This    : in out Instance'Class;
      Operand : Pdp11.ISA.Operand_Type)
      return Vector_96;

   procedure Set_Vector_Operand_Value
     (This    : in out Instance'Class;
      Operand : Pdp11.ISA.Operand_Type;
      Value   : Vector_96);

   procedure Write_Register
     (This    : in out Instance'Class;
      Register : Pdp11.ISA.Register_Index;
      Word     : Boolean;
      Value    : Word_16);

   procedure Set_NZ
     (This    : in out Instance'Class;
      Word     : Boolean;
      Value    : Word_16);

   procedure Set_VC
     (This    : in out Instance'Class;
      Word     : Boolean;
      X, Y, Z  : Word_16);

   procedure Double_Operand
     (This    : in out Instance'Class;
      Op      : Pdp11.ISA.Double_Operand_Instruction;
      Word    : Boolean;
      Src     : Pdp11.ISA.Operand_Type;
      Dst     : Pdp11.ISA.Operand_Type);

   procedure Register_Double_Operand
     (This : in out Instance'Class;
      Op      : Pdp11.ISA.Register_Double_Operand_Instruction;
      Src     : Pdp11.ISA.Operand_Type;
      Dst     : Pdp11.ISA.Register_Index);

   procedure Single_Operand
     (This : in out Instance'Class;
      Op      : Pdp11.ISA.Single_Operand_Instruction;
      Word    : Boolean;
      Dst     : Pdp11.ISA.Operand_Type);

   procedure Branch
     (This     : in out Instance'Class;
      Instruction : Pdp11.ISA.Branch_Instruction;
      Offset      : Word_8);

   procedure Float_Format_1
     (This     : in out Instance'Class;
      Instruction : ISA.Floating_Point_F1;
      AC          : ISA.FP_Register_Index;
      Operand     : ISA.Operand_Type);

   procedure Float_Format_2
     (This     : in out Instance'Class;
      Instruction : ISA.Floating_Point_F2;
      Operand     : ISA.Operand_Type);

   procedure Vector_Format_1
     (This     : in out Instance'Class;
      Instruction : ISA.Vector_F1;
      VAC         : ISA.V_Register_Index;
      Operand     : ISA.Operand_Type);

   procedure Vector_Format_2
     (This     : in out Instance'Class;
      Instruction : ISA.Vector_F2;
      VAC_1       : ISA.V_Register_Index;
      VAC_2       : ISA.V_Register_Index);

   procedure Vector_Format_3
     (This     : in out Instance'Class;
      Instruction : ISA.Vector_F3;
      VAC         : ISA.V_Register_Index);

   procedure Raise_Bus_Error
     (This : Instance'Class;
      Address : Address_Type;
      Write   : Boolean)
     with No_Return, Unreferenced;

   --  procedure Raise_ISA_Error
   --    (This : Instance'Class;
   --     Address : Address_Type;
   --     IR      : Word_16)
   --  is null;

   ----------------
   -- Add_Device --
   ----------------

   overriding procedure Add_Device
     (This     : in out Instance;
      Device   : not null access Pdp11.Devices.Instance'Class)
   is
   begin
      This.Memory.Add_Device (Device);
      This.Devices.Append (Pdp11.Devices.Reference (Device));
   end Add_Device;

   ------------
   -- Branch --
   ------------

   procedure Branch
     (This     : in out Instance'Class;
      Instruction : Pdp11.ISA.Branch_Instruction;
      Offset      : Word_8)
   is
      use Pdp11.ISA;
      Taken : Boolean := False;
   begin
      case Instruction is
         when I_BR =>
            Taken := True;
         when I_BEQ =>
            Taken := This.Z;
         when I_BLT =>
            Taken := This.N xor This.V;
         when I_BLE =>
            Taken := This.Z or else (This.N xor This.V);
         when I_BMI =>
            Taken := This.N;
         when I_BLOS =>
            Taken := This.C or else This.Z;
         when I_BVS =>
            Taken := This.V;
         when I_BCS =>
            Taken := This.C;
         when I_BNE =>
            Taken := not This.Z;
         when I_BGE =>
            Taken := not (This.N xor This.V);
         when I_BGT =>
            Taken := not (This.Z or else (This.N xor This.V));
         when I_BPL =>
            Taken := not This.N;
         when I_BHI =>
            Taken := not (This.C or else This.Z);
         when I_BVC =>
            Taken := not This.V;
         when I_BCC =>
            Taken := not This.C;
      end case;
      if Taken then
         This.Current_Timing := This.Current_Timing + 0.9;
         declare
            PC : Word_16 renames This.Rs (7);
            New_PC : constant Word_16 :=
                       (if Offset < 128
                        then PC + 2 * Word_16 (Offset)
                        else PC - 2 * (256 - Word_16 (Offset)));
         begin
            PC := New_PC;
         end;
      end if;
   end Branch;

   -------------------
   -- Clear_Devices --
   -------------------

   overriding procedure Clear_Devices
     (This : in out Instance)
   is
   begin
      This.Memory.Clear_Devices;
      This.Devices.Clear;
   end Clear_Devices;

   -----------------
   -- Clock_Image --
   -----------------

   function Clock_Image (This : Instance'Class) return String is
      It : Natural := Natural (This.Clock * 100.0);
      Img : String := "            ";
      Dec : Integer := 2;
   begin
      for Ch of reverse Img loop
         if Dec = 0 then
            Ch := '.';
         else
            Ch := Character'Val (48 + It mod 10);
            It := It / 10;
            exit when It = 0 and then Dec < 0;
         end if;
         Dec := Dec - 1;
      end loop;
      return Img;
   end Clock_Image;

   ------------
   -- Create --
   ------------

   procedure Create
     (This : in out Instance'Class;
      Memory : not null access
        Pdp11.Addressable.Memory.Root_Memory_Type'Class)
   is
   begin
      This.Memory := Pdp11.Addressable.Memory.Memory_Reference (Memory);
   end Create;

   --------------------
   -- Double_Operand --
   --------------------

   procedure Double_Operand
     (This : in out Instance'Class;
      Op      : Pdp11.ISA.Double_Operand_Instruction;
      Word    : Boolean;
      Src     : Pdp11.ISA.Operand_Type;
      Dst     : Pdp11.ISA.Operand_Type)
   is
      use Pdp11.ISA;
      Timing   : constant Microsecond_Duration :=
                   Src_Operand_Timing (Op, Src)
                   + Dst_Operand_Timing (Op, Dst);
      X        : constant Word_16 := This.Get_Operand_Value (Src, Word);
      Dst_Addr : constant Address_Type :=
        (if Is_Register_Operand (Dst) then 0
         else This.Get_Operand_Address (Dst, Word));
      Y        : Word_16 := 0;

      procedure Store (Z : Word_16);

      -----------
      -- Store --
      -----------

      procedure Store (Z : Word_16) is
      begin
         if Is_Register_Operand (Dst) then
            This.Write_Register (Dst.Register, Word, Z);
         elsif Word then
            This.Set_Word_16 (Dst_Addr, Z);
         else
            This.Set_Word_8 (Dst_Addr, Word_8 (Z mod 256));
         end if;

         if Trace_Execution then
            Ada.Text_IO.Put
              (" "
               & (if Word then Hex_Image (Z)
                 else Hex_Image (Word_8 (Z mod 256)))
               & " -> "
              & (if Is_Register_Operand (Dst)
                 then Register_Image (Dst.Register)
                 else Hex_Image (Word_16 (Dst_Addr))));
         end if;
      end Store;

   begin

      if Op /= I_MOV then
         if Is_Register_Operand (Dst) then
            Y := This.Rs (Dst.Register);
         else
            This.Get_Word_16 (Dst_Addr, Y);
         end if;
      end if;

      case Op is
         when I_MOV =>
            Store (X);
            This.Set_NZ (Word, X);
            This.V := False;

         when I_CMP =>
            declare
               Z : constant Word_16 := X - Y;
            begin
               This.Set_NZ (Word, Z);
               This.Set_VC (Word, X, (not Y + 1), Z);
            end;

         when I_BIT =>
            declare
               Z : constant Word_16 := X and Y;
            begin
               This.Set_NZ (Word, Z);
               This.V := False;
            end;

         when I_BIC =>
            declare
               Z : constant Word_16 := (not X) and Y;
            begin
               Store (Z);
               This.Set_NZ (Word, Z);
               This.V := False;
            end;

         when I_BIS =>
            declare
               Z : constant Word_16 := X or Y;
            begin
               Store (Z);
               This.Set_NZ (Word, Z);
               This.V := False;
            end;

         when I_ADD =>
            declare
               Z : constant Word_16 := X + Y;
            begin
               Store (Z);
               This.Set_NZ (Word, Z);
               This.Set_VC (Word, X, Y, Z);
            end;

         when I_SUB =>
            declare
               Z : constant Word_16 := Y - X;
            begin
               Store (Z);
               This.Set_NZ (Word, Z);
               This.Set_VC (Word, X, (not Y) + 1, Z);
            end;
      end case;
      This.Current_Timing := This.Current_Timing + Timing;
   end Double_Operand;

   ------------------------------
   -- Execute_Next_Instruction --
   ------------------------------

   procedure Execute_Next_Instruction
     (This : in out Instance'Class)
   is
      use Pdp11.ISA;
   begin
      if not This.Started then
         This.Get_Word_16 (16#FFFE#, This.Rs (7));
         This.Started := True;
         This.Clock := 0.0;
      end if;

      declare
         Start : constant Microsecond_Duration := This.Clock;
      begin
         This.Next;

         declare
            Finish  : constant Microsecond_Duration := This.Clock;
            Elapsed : constant Microsecond_Duration := Finish - Start;
         begin
            for Device of This.Devices loop
               Device.Tick (Elapsed, This'Unchecked_Access);
            end loop;
         end;
      end;

      if Limit_Speed then
         declare
            use type Ada.Calendar.Time;
            Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
            Elapsed_Real : constant Duration := Now - This.Start_Time;
            Elapsed_Machine : constant Duration :=
                                Duration
                                  (This.Clock - This.Start_Clock)
                                  / 1.0e6;
         begin
            if Elapsed_Machine > Elapsed_Real then
               delay Elapsed_Machine - Elapsed_Real;
            end if;
         end;
      end if;

      while This.Waiting loop
         This.Clock := This.Clock + 1.0;
         for Device of This.Devices loop
            Device.Tick (1.0, This'Unchecked_Access);
         end loop;
      end loop;

   end Execute_Next_Instruction;

   ---------------------
   -- Execute_Quantum --
   ---------------------

   procedure Execute_Quantum
     (This    : in out Instance'Class;
      Quantum : Pdp11.ISA.Microsecond_Duration)
   is
      Used : Pdp11.ISA.Microsecond_Duration with Unreferenced;
   begin
      This.Execute_Quantum (Quantum, Used);
   end Execute_Quantum;

   ---------------------
   -- Execute_Quantum --
   ---------------------

   procedure Execute_Quantum
     (This : in out Instance'Class;
      Quantum : ISA.Microsecond_Duration;
      Used    : out ISA.Microsecond_Duration)
   is
      use Pdp11.ISA;
      Start_Clock : Microsecond_Duration;
      End_Clock   : Microsecond_Duration;
   begin
      Trace_Execution := Pdp11.Options.Trace;

      Start_Clock := This.Clock;
      End_Clock   := This.Clock + Quantum;

      This.Start_Time := Ada.Calendar.Clock;
      This.Start_Clock := This.Clock;

      while This.Clock < End_Clock loop
         This.Execute_Next_Instruction;
      end loop;

      Used := This.Clock - Start_Clock;

   exception
      when E : Pdp11.Addressable.Bad_Address =>
         Ada.Text_IO.Put_Line
           (Ada.Exceptions.Exception_Message (E));
         Used := This.Clock - Start_Clock;
   end Execute_Quantum;

   -----------------
   -- Flags_Image --
   -----------------

   function Flags_Image (This : Instance'Class) return String is
      function Img (Ch : Character;
                    Set : Boolean)
                    return Character
      is (if Set then Ch else '-');

   begin
      return (Img ('N', This.N),
              Img ('Z', This.Z),
              Img ('V', This.V),
              Img ('C', This.C));
   end Flags_Image;

   --------------------
   -- Float_Format_1 --
   --------------------

   procedure Float_Format_1
     (This     : in out Instance'Class;
      Instruction : ISA.Floating_Point_F1;
      AC          : ISA.FP_Register_Index;
      Operand     : ISA.Operand_Type)
   is
      use Pdp11.ISA;

      procedure Set_Flags
        (Value : Float_32);

      procedure Operate
        (Src : Float_32;
         Dst : in out Float_32);

      -------------
      -- Operate --
      -------------

      procedure Operate
        (Src : Float_32;
         Dst : in out Float_32)
      is
      begin
         case Instruction is
            when I_MULF =>
               Dst := Dst * Src;
            when I_MODF =>
               Dst := Float_32 (Integer (Dst) mod Integer (Src));
            when I_ADDF =>
               Dst := Dst + Src;
            when I_LDF =>
               Dst := Src;
            when I_SUBF =>
               Dst := Dst - Src;
            when I_CMPF =>
               Set_Flags (Dst - Src);
            when I_STF =>
               pragma Assert (False, "STF should have been handled elsewhere");
            when I_DIVF =>
               Dst := Dst / Src;
         end case;

         if Instruction /= I_CMPF then
            Set_Flags (Dst);
         end if;
      end Operate;

      ---------------
      -- Set_Flags --
      ---------------

      procedure Set_Flags
        (Value : Float_32)
      is
      begin
         This.Z := Value = 0.0;
         This.N := Value < 0.0;
      end Set_Flags;

   begin
      if Operand.Mode = Register_Mode and then not Operand.Deferred then
         if Operand.Register > Register_Index (FP_Register_Index'Last) then
            raise ISA_Error;
         end if;

         if Instruction = I_STF then
            This.ACs (FP_Register_Index (Operand.Register)) :=
              This.ACs (AC);
         else
            Operate
              (This.ACs (FP_Register_Index (Operand.Register)),
               This.ACs (AC));
         end if;

         if Trace_Execution then
            Ada.Text_IO.Put_Line
              ("ac" & Character'Val (48 + AC) & " <- "
               & This.ACs (AC)'Image);
         end if;

      elsif Instruction = I_STF then
         This.Set_Float_Operand_Value
           (Operand, This.ACs (AC));
      else
         Operate (This.Get_Float_Operand_Value (Operand),
                  This.ACs (AC));
      end if;
   end Float_Format_1;

   --------------------
   -- Float_Format_2 --
   --------------------

   procedure Float_Format_2
     (This     : in out Instance'Class;
      Instruction : ISA.Floating_Point_F2;
      Operand     : ISA.Operand_Type)
   is
      use ISA;
   begin
      case Instruction is
         when I_CLRF =>
            This.Set_Float_Operand_Value (Operand, 0.0);
         when I_TSTF =>
            null;
         when I_ABSF =>
            declare
               procedure Update (X : in out Float_32);

               ------------
               -- Update --
               ------------

               procedure Update (X : in out Float_32) is
               begin
                  X := abs X;
               end Update;

            begin
               This.Update_Float_Operand_Value
                 (Operand, Update'Access);
            end;

         when I_NEGF =>
            declare
               procedure Update (X : in out Float_32);

               ------------
               -- Update --
               ------------

               procedure Update (X : in out Float_32) is
               begin
                  X := -X;
               end Update;

            begin
               This.Update_Float_Operand_Value
                 (Operand, Update'Access);
            end;
      end case;

   end Float_Format_2;

   ------------------
   -- Get_Float_32 --
   ------------------

   overriding procedure Get_Float_32
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Float_32)
   is
   begin
      This.Memory.Get_Float_32 (Address, Value);
   end Get_Float_32;

   -----------------------------
   -- Get_Float_Operand_Value --
   -----------------------------

   function Get_Float_Operand_Value
     (This : in out Instance'Class;
      Operand : Pdp11.ISA.Operand_Type)
      return Float_32
   is
      use ISA;
   begin
      if Operand.Mode = Register_Mode
        and then not Operand.Deferred
      then
         if Operand.Register > Register_Index (FP_Register_Index'Last) then
            raise ISA_Error;
         end if;
         return This.ACs (FP_Register_Index (Operand.Register));
      else
         declare
            A  : constant Address_Type :=
                   This.Get_Operand_Address (Operand, 4);
            V  : Float_32;
         begin
            This.Get_Float_32 (A, V);
            return V;
         end;
      end if;
   end Get_Float_Operand_Value;

   -------------------------
   -- Get_Operand_Address --
   -------------------------

   function Get_Operand_Address
     (This : in out Instance'Class;
      Operand : Pdp11.ISA.Operand_Type;
      Size    : Positive)
      return Address_Type
   is
      use Pdp11.ISA;
      R  : Word_16 renames This.Rs (Operand.Register);
      PC : Word_16 renames This.Rs (7);
      A  : Address_Type := 0;
   begin
      case Operand.Mode is
         when Register_Mode =>
            pragma Assert (Operand.Deferred);
            return Address_Type (R);

         when Autoincrement_Mode =>
            A := Address_Type (R);
            if Size = 2
              or else Operand.Deferred
              or else (Size = 1 and then Operand.Register >= 6)
            then
               R := R + 2;
            else
               R := R + Word_16 (Size);
            end if;

         when Autodecrement_Mode =>
            if Size = 2
              or else Operand.Deferred
              or else (Size = 1 and then Operand.Register >= 6)
            then
               R := R - 2;
            else
               R := R - Word_16 (Size);
            end if;
            A := Address_Type (R);

         when Index_Mode =>
            declare
               Index : Word_16;
            begin
               This.Get_Word_16 (Address_Type (PC), Index);
               PC := PC + 2;
               A := Address_Type (R + Index);
            end;
      end case;

      if Operand.Deferred then
         declare
            V : Word_16;
         begin
            This.Get_Word_16 (A, V);
            A := Address_Type (V);
         end;
      end if;

      return A;
   end Get_Operand_Address;

   -----------------------
   -- Get_Operand_Value --
   -----------------------

   function Get_Operand_Value
     (This : in out Instance'Class;
      Operand : Pdp11.ISA.Operand_Type;
      Word    : Boolean)
      return Word_16
   is
      use Pdp11.ISA;
   begin
      if Pdp11.ISA.Is_Register_Operand (Operand) then
         return This.Rs (Operand.Register);
      else
         declare
            Address : constant Address_Type :=
                        This.Get_Operand_Address (Operand, Word);
         begin
            if Trace_Execution then
               Ada.Text_IO.Put (" (" & Hex_Image (Word_16 (Address)) & ")");
            end if;

            if Address mod 2 = 1 then
               This.Current_Timing := This.Current_Timing + 0.6;
            end if;

            if Word then
               declare
                  W : Word_16;
               begin
                  This.Get_Word_16 (Address, W);
                  return W;
               end;
            else
               declare
                  W : Word_8;
               begin
                  This.Get_Word_8 (Address, W);
                  return Word_16 (W);
               end;
            end if;
         end;
      end if;
   end Get_Operand_Value;

   function Get_PS
     (This : Instance'Class)
      return Word_16
   is
      PSW : Word_16 := Word_16 (This.Priority);

      procedure Get (X : Boolean);

      ---------
      -- Get --
      ---------

      procedure Get (X : Boolean) is
      begin
         PSW := PSW * 2 + Boolean'Pos (X);
      end Get;

   begin
      Get (This.T);
      Get (This.N);
      Get (This.Z);
      Get (This.V);
      Get (This.C);

      return PSW;
   end Get_PS;

   ------------------------------
   -- Get_Vector_Operand_Value --
   ------------------------------

   function Get_Vector_Operand_Value
     (This : in out Instance'Class;
      Operand : Pdp11.ISA.Operand_Type)
      return Vector_96
   is
      use ISA;
   begin
      if Operand.Mode = Register_Mode
        and then not Operand.Deferred
      then
         if Operand.Register > Register_Index (V_Register_Index'Last) then
            raise ISA_Error;
         end if;
         return This.VRs (V_Register_Index (Operand.Register));
      else
         declare
            A  : Address_Type :=
                   This.Get_Operand_Address (Operand, 4);
            X  : Float_32;
            Y  : Float_32;
            Z  : Float_32;
         begin
            This.Get_Float_32 (A, X);
            if Operand.Mode in Autodecrement_Mode | Autoincrement_Mode
              and then not Operand.Deferred
            then
               A := This.Get_Operand_Address (Operand, 4);
            else
               A := A + 4;
            end if;

            This.Get_Float_32 (A, Y);

            if Operand.Mode in Autodecrement_Mode | Autoincrement_Mode
              and then not Operand.Deferred
            then
               A := This.Get_Operand_Address (Operand, 4);
            else
               A := A + 4;
            end if;

            This.Get_Float_32 (A, Z);

            return (X, Y, Z);
         end;
      end if;
   end Get_Vector_Operand_Value;

   ----------------
   -- Get_Word_8 --
   ----------------

   overriding procedure Get_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_8)
   is
   begin
      This.Memory.Get_Word_8 (Address, Value);
   end Get_Word_8;

   -----------------
   -- Get_Word_16 --
   -----------------

   overriding procedure Get_Word_16
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_16)
   is
   begin
      This.Memory.Get_Word_16 (Address, Value);
   end Get_Word_16;

   ---------------
   -- Interrupt --
   ---------------

   overriding procedure Interrupt
     (This  : in out Instance;
      Priority : Interrupt_Priority_Type;
      Vector   : Address_Type)
   is
      procedure Push (X : Word_16);

      ----------
      -- Push --
      ----------

      procedure Push (X : Word_16) is
      begin
         This.Rs (6) := This.Rs (6) - 2;
         This.Set_Word_16 (Address_Type (This.Rs (6)), X);
      end Push;

   begin
      if Priority > This.Priority then
         --  Ada.Text_IO.Put_Line
         --    ("interrupt: priority" & Priority'Image
         --     & ", vector " & Pdp11.Images.Hex_Image (Word_16 (Vector)));
         Push (This.Get_PS);
         Push (This.Rs (7));
         This.Get_Word_16 (Vector, This.Rs (7));
         declare
            PS : Word_16;
         begin
            This.Get_Word_16 (Vector + 2, PS);
            This.Set_PS (PS);
         end;
         This.Current_Timing :=
           Pdp11.ISA."+" (This.Current_Timing, 7.2);
         This.Waiting := False;
      end if;
   end Interrupt;

   ----------
   -- Next --
   ----------

   procedure Next (This : in out Instance'Class) is
      use Pdp11.ISA;
      PC : Word_16 renames This.Rs (7);
      SP : Word_16 renames This.Rs (6);
      IR : Word_16;
      Rec : Instruction_Record;
   begin

      This.Get_Word_16 (Address_Type (PC), IR);
      Rec := Decode (IR);

      if Trace_Execution then
         Ada.Text_IO.Put
           (Clock_Image (This)
            & " "
            & Flags_Image (This)
            & " "
            & Hex_Image (PC) & ": " & Octal_Image (IR)
            & " " & Image (Rec));
      end if;

      PC := PC + 2;

      This.Current_Instruction := Rec.Instruction;
      This.Current_Timing :=
        ISA.Basic_Timing (Rec.Instruction);

      case Rec.Instruction is
         when Double_Operand_Instruction =>
            This.Double_Operand
              (Rec.Instruction, Rec.Word, Rec.Src, Rec.Dst);

         when Register_Double_Operand_Instruction =>
            This.Register_Double_Operand
              (Rec.Instruction, Rec.Src, Rec.Dst.Register);

         when Single_Operand_Instruction =>
            This.Single_Operand (Rec.Instruction, Rec.Word, Rec.Dst);

         when Branch_Instruction =>
            This.Branch (Rec.Instruction, Rec.Offset);

         when I_SOB =>
            This.Rs (Rec.Src.Register) :=
              This.Rs (Rec.Src.Register) - 1;
            if This.Rs (Rec.Src.Register) /= 0 then
               declare
                  PC     : Word_16 renames This.Rs (7);
               begin
                  PC := PC - 2 * Word_16 (Rec.Offset);
               end;
            end if;

         when I_JMP =>
            PC := Word_16 (This.Get_Operand_Address (Rec.Dst, True));

         when I_JSR =>
            declare
               Destination : constant Address_Type :=
                               This.Get_Operand_Address (Rec.Dst, True);
            begin
               if Rec.Src.Register /= 7 then
                  This.Rs (Rec.Src.Register) := PC;
               end if;
               SP := SP - 2;
               This.Set_Word_16
                 (Address_Type (SP), This.Rs (Rec.Src.Register));
               if Trace_Execution then
                  Ada.Text_IO.Put
                    (" (" & Hex_Image (This.Rs (Rec.Src.Register))
                     & " -> " & Hex_Image (SP) & ")");
               end if;

               PC := Word_16 (Destination);
            end;

         when I_RTS =>
            PC := This.Rs (Rec.Src.Register);

            This.Get_Word_16 (Address_Type (SP), This.Rs (Rec.Src.Register));

            if Trace_Execution then
               Ada.Text_IO.Put
                 (" (" & Hex_Image (SP) & " "
                  & Hex_Image (This.Rs (Rec.Src.Register))
                  & " -> " & Register_Image (Rec.Src.Register)
                  & ")");
            end if;
            SP := SP + 2;

         when I_CCC | I_SCC =>
            declare
               Set : constant Boolean := Rec.Instruction = I_SCC;
            begin
               if Rec.N then
                  This.N := Set;
               end if;
               if Rec.Z then
                  This.Z := Set;
               end if;
               if Rec.C then
                  This.C := Set;
               end if;
               if Rec.V then
                  This.V := Set;
               end if;
            end;

         when I_HALT =>
            raise Halted with "halted at " & Hex_Image (PC);

         when I_WAIT =>
            --  Ada.Text_IO.Put_Line ("waiting for interrupt ...");
            This.Waiting := True;

         when I_RTI =>
            This.Get_Word_16 (Address_Type (SP), PC);
            SP := SP + 2;
            declare
               PS : Word_16;
            begin
               This.Get_Word_16 (Address_Type (SP), PS);
               This.Set_PS (PS);
            end;
            SP := SP + 2;

         when I_IOT =>
            SP := SP - 2;
            This.Set_Word_16 (Address_Type (SP), This.Get_PS);
            SP := SP - 2;
            This.Set_Word_16 (Address_Type (SP), PC);
            This.Get_Word_16 (8#20#, PC);
            declare
               PS : Word_16;
            begin
               This.Get_Word_16 (8#22#, PS);
               This.Set_PS (PS);
            end;

         when I_RESET =>
            null;

         when I_EMT =>
            SP := SP - 2;
            This.Set_Word_16 (Address_Type (SP), This.Get_PS);
            SP := SP - 2;
            This.Set_Word_16 (Address_Type (SP), PC);
            This.Set_Word_16 (Address_Type (SP), PC);
            This.Get_Word_16 (8#30#, PC);
            declare
               PS : Word_16;
            begin
               This.Get_Word_16 (8#32#, PS);
               This.Set_PS (PS);
            end;

         when I_TRAP =>
            SP := SP - 2;
            This.Set_Word_16 (Address_Type (SP), This.Get_PS);
            SP := SP - 2;
            This.Set_Word_16 (Address_Type (SP), PC);
            This.Get_Word_16 (8#34#, PC);
            declare
               PS : Word_16;
            begin
               This.Get_Word_16 (8#36#, PS);
               This.Set_PS (PS);
            end;

         when Floating_Point_F1 =>
            This.Float_Format_1 (Rec.Instruction, Rec.FAC, Rec.F_Operand);

         when Floating_Point_F2 =>
            This.Float_Format_2 (Rec.Instruction, Rec.F_Operand);

         when I_INVF =>
            declare
               procedure Update (X : in out Float_32);

               ------------
               -- Update --
               ------------

               procedure Update (X : in out Float_32) is
               begin
                  if X = 0.0 then
                     raise Division_By_Zero;
                  end if;
                  X := 1.0 / X;
               end Update;

            begin
               This.Update_Float_Operand_Value
                 (Rec.F_Operand, Update'Access);
            end;

         when Vector_F1 =>
            This.Vector_Format_1
              (Rec.Instruction, Rec.VAC_1, Rec.V_Operand);

         when Vector_F2 =>
            This.Vector_Format_2 (Rec.Instruction, Rec.VAC_1, Rec.VAC_2);

         when Vector_F3 =>
            This.Vector_Format_3 (Rec.Instruction, Rec.VAC_1);

      end case;

      if Trace_Execution then
         Ada.Text_IO.New_Line;
      end if;

      This.Clock := This.Clock + This.Current_Timing;

   end Next;

   ---------------------
   -- Raise_Bus_Error --
   ---------------------

   procedure Raise_Bus_Error
     (This : Instance'Class;
      Address : Address_Type;
      Write   : Boolean)
   is
   begin
      raise Pdp11.Addressable.Bad_Address with
        "Bus error at PC "
        & Hex_Image (This.Rs (7))
        & " while "
        & (if Write then "writing" else "reading")
        & " location "
        & Hex_Image (Word_16 (Address));
   end Raise_Bus_Error;

   -----------------------------
   -- Register_Double_Operand --
   -----------------------------

   procedure Register_Double_Operand
     (This : in out Instance'Class;
      Op      : Pdp11.ISA.Register_Double_Operand_Instruction;
      Src     : Pdp11.ISA.Operand_Type;
      Dst     : Pdp11.ISA.Register_Index)
   is
      use Pdp11.ISA;
      T32 : Word_32;
   begin
      case Op is
         when I_MUL =>
            T32 := Word_32 (This.Rs (Dst))
              * Word_32 (This.Get_Operand_Value (Src, True));
            This.Rs (Dst) := Word_16 (T32 mod 65536);
            if Dst mod 2 = 0 then
               This.Rs (Dst + 1) := Word_16 (T32 / 65536);
            end if;
            This.N := T32 >= 2 ** 31;
            This.Z := T32 = 0;
            This.C := T32 in 2 ** 15 .. 2 ** 32 - 2 ** 15;
         when I_DIV =>
            null;

         when I_ASH =>
            null;

         when I_ASHC =>
            null;

         when I_XOR =>
            null;
      end case;

   end Register_Double_Operand;

   ------------
   -- Report --
   ------------

   procedure Report (This : Instance'Class) is
   begin
      Ada.Text_IO.Put_Line
        (" R0   R1   R2   R3   R4   R5   SP   PC  NZVC   Clock");
      for R of This.Rs loop
         Ada.Text_IO.Put (Hex_Image (R) & " ");
      end loop;
      Ada.Text_IO.Put (if This.N then "N" else "-");
      Ada.Text_IO.Put (if This.Z then "Z" else "-");
      Ada.Text_IO.Put (if This.V then "V" else "-");
      Ada.Text_IO.Put (if This.C then "C" else "-");
      Ada.Text_IO.Put (This.Clock_Image);
      Ada.Text_IO.New_Line;

      declare
         use type Ada.Calendar.Time;
         use type Pdp11.ISA.Microsecond_Duration;
         Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         Elapsed_Real : constant Duration := Now - This.Start_Time;
         Elapsed_Machine : constant Duration :=
                             Duration
                               (This.Clock - This.Start_Clock)
                               / 1.0e6;
      begin
         Ada.Text_IO.Put_Line
           ("elapsed: machine" & Elapsed_Machine'Image
            & "; real" & Elapsed_Real'Image);
      end;

   end Report;

   ------------------
   -- Set_Float_32 --
   ------------------

   overriding procedure Set_Float_32
     (This : in out Instance;
      Address : Address_Type;
      Value   : Float_32)
   is
   begin
      This.Memory.Set_Float_32 (Address, Value);
   end Set_Float_32;

   -----------------------------
   -- Set_Float_Operand_Value --
   -----------------------------

   procedure Set_Float_Operand_Value
     (This : in out Instance'Class;
      Operand : Pdp11.ISA.Operand_Type;
      Value   : Float_32)
   is
      use ISA;
   begin
      if Operand.Mode = Register_Mode
        and then not Operand.Deferred
      then
         if Operand.Register <= Register_Index (FP_Register_Index'Last) then
            This.ACs (FP_Register_Index (Operand.Register)) := Value;
         else
            raise ISA_Error;
         end if;
         return;
      end if;

      declare
         W32 : constant Word_32 :=
           Conversions.As_Word_32 (Value);
         Lo  : constant Word_16 := Word_16 (W32 mod 65536);
         Hi  : constant Word_16 := Word_16 (W32 / 65536);
         A   : Address_Type := This.Get_Operand_Address (Operand, True);
      begin

         This.Set_Word_16 (A, Lo);

         if Operand.Mode in Autodecrement_Mode | Autoincrement_Mode
           and then not Operand.Deferred
         then
            A := This.Get_Operand_Address (Operand, True);
         else
            A := A + 2;
         end if;

         This.Set_Word_16 (A, Hi);
      end;
   end Set_Float_Operand_Value;

   ------------
   -- Set_NZ --
   ------------

   procedure Set_NZ
     (This    : in out Instance'Class;
      Word     : Boolean;
      Value    : Word_16)
   is
   begin
      if Word then
         This.N := Value >= 2 ** 15;
         This.Z := Value = 0;
      else
         This.N := Value mod 256 >= 128;
         This.Z := Value mod 256 = 0;
      end if;
   end Set_NZ;

   ------------
   -- Set_PS --
   ------------

   procedure Set_PS
     (This : in out Instance'Class;
      Value   : Word_16)
   is
      It : Word_16 := Value;

      procedure Set (X : in out Boolean);

      ---------
      -- Set --
      ---------

      procedure Set (X : in out Boolean) is
      begin
         X := It mod 2 = 1;
         It := It / 2;
      end Set;

   begin
      Set (This.C);
      Set (This.V);
      Set (This.Z);
      Set (This.N);
      Set (This.T);
      This.Priority := Priority_Type (It mod 8);
   end Set_PS;

   ------------------
   -- Set_Register --
   ------------------

   procedure Set_Register
     (This    : in out Instance'Class;
      Register : Machine_Register;
      Value    : Word_16)
   is
   begin
      This.Rs (Register) := Value;
      This.Started := True;
   end Set_Register;

   ------------
   -- Set_VC --
   ------------

   procedure Set_VC
     (This    : in out Instance'Class;
      Word     : Boolean;
      X, Y, Z  : Word_16)
   is
      function Negative (V : Word_16) return Boolean
      is (if Word then V / 2 ** 15 = 1 else V mod 256 / 128 = 1);

      XN : constant Boolean := Negative (X);
      YN : constant Boolean := Negative (Y);
      ZN : constant Boolean := Negative (Z);

   begin
      This.V := XN = YN and then XN /= ZN;
      This.C :=
        (if Word then (2 ** 16 - 1) - X > Y
         else (2 ** 8 - 1) - (X mod 256) > (Y mod 256));
   end Set_VC;

   ------------------------------
   -- Set_Vector_Operand_Value --
   ------------------------------

   procedure Set_Vector_Operand_Value
     (This : in out Instance'Class;
      Operand : Pdp11.ISA.Operand_Type;
      Value   : Vector_96)
   is
      use ISA;
   begin
      if Operand.Mode = Register_Mode
        and then not Operand.Deferred
      then
         if Operand.Register > Register_Index (V_Register_Index'Last) then
            raise ISA_Error;
         end if;
         This.VRs (V_Register_Index (Operand.Register)) := Value;
      else
         declare
            A  : Address_Type :=
                   This.Get_Operand_Address (Operand, 4);
         begin
            This.Set_Float_32 (A, Value.X);

            if Operand.Mode in Autodecrement_Mode | Autoincrement_Mode
              and then not Operand.Deferred
            then
               A := This.Get_Operand_Address (Operand, 4);
            else
               A := A + 4;
            end if;

            This.Set_Float_32 (A, Value.Y);

            if Operand.Mode in Autodecrement_Mode | Autoincrement_Mode
              and then not Operand.Deferred
            then
               A := This.Get_Operand_Address (Operand, 4);
            else
               A := A + 4;
            end if;

            This.Set_Float_32 (A, Value.Z);

         end;
      end if;
   end Set_Vector_Operand_Value;

   ----------------
   -- Set_Word_8 --
   ----------------

   overriding procedure Set_Word_8
     (This : in out Instance;
      Address : Address_Type;
      Value   : Word_8)
   is
   begin
      This.Memory.Set_Word_8 (Address, Value);
   end Set_Word_8;

   -----------------
   -- Set_Word_16 --
   -----------------

   overriding procedure Set_Word_16
     (This : in out Instance;
      Address : Address_Type;
      Value   : Word_16)
   is
   begin
      This.Memory.Set_Word_16 (Address, Value);
   end Set_Word_16;

   --------------------
   -- Single_Operand --
   --------------------

   procedure Single_Operand
     (This : in out Instance'Class;
      Op      : Pdp11.ISA.Single_Operand_Instruction;
      Word    : Boolean;
      Dst     : Pdp11.ISA.Operand_Type)
   is
      use Pdp11.ISA;
      Timing   : constant Microsecond_Duration :=
                   Dst_Operand_Timing (Op, Dst);
      Dst_Addr : constant Address_Type :=
                   (if Is_Register_Operand (Dst) then 0
                    else This.Get_Operand_Address (Dst, Word));
      Y        : Word_16 := This.Rs (Dst.Register);

      procedure Result
        (Z         : Word_16;
         Store     : Boolean := True;
         Set_Flags : Boolean := True);

      ------------
      -- Result --
      ------------

      procedure Result
        (Z         : Word_16;
         Store     : Boolean := True;
         Set_Flags : Boolean := True)
      is
      begin
         if Trace_Execution then
            Ada.Text_IO.Put
              (" "
               & (if Word then Hex_Image (Z)
                 else Hex_Image (Word_8 (Z mod 256)))
               & (if Store
                 then " -> "
                 & (if Is_Register_Operand (Dst)
                   then Register_Image (Dst.Register)
                   else Hex_Image (Word_16 (Dst_Addr)))
                 else ""));
         end if;

         if Store then
            if Is_Register_Operand (Dst) then
               This.Write_Register (Dst.Register, Word, Z);
            elsif Word then
               This.Set_Word_16 (Dst_Addr, Z);
            else
               This.Set_Word_8 (Dst_Addr, Word_8 (Z mod 256));
            end if;
         end if;

         if Set_Flags then
            This.Set_NZ (Word, Z);
         end if;

      end Result;

   begin

      if not Is_Register_Operand (Dst) then
         This.Get_Word_16 (Dst_Addr, Y);
      end if;

      case Op is
         when I_CLR =>
            Result (0);

         when I_COM =>
            Result (not Y);

         when I_INC =>
            Result (Y + 1);

         when I_DEC =>
            Result (Y - 1);

         when I_NEG =>
            Result ((not Y) + 1);

         when I_ADC =>
            Result (Y + Boolean'Pos (This.C));

         when I_SBC =>
            Result (Y - Boolean'Pos (This.C));

         when I_TST =>
            Result (Y, Store => False);

         when I_ROR =>
            declare
               C : constant Word_16 :=
                     (if This.C then
                        (if Word then 32768 else 128)
                      else 0);
            begin
               Result (C + Y / 2);
               This.C := Y mod 2 = 1;
            end;

         when I_ROL =>
            declare
               C : constant Word_16 :=
                     (if This.C then 1 else 0);
            begin
               Result (C + Y * 2);
               if Word then
                  This.C := Y >= 32768;
               else
                  This.C := Y mod 256 >= 128;
               end if;
            end;

         when I_ASR =>
            Result (Y / 2);

         when I_ASL =>
            Result (Y * 2);

         when I_MARK =>
            null;

         when I_MTPS =>
            This.Set_PS (Y);

         when I_MFPI =>
            null;

         when I_MFPD =>
            null;

         when I_MTPI =>
            null;

         when I_MTPD =>
            null;

         when I_SXT =>
            null;

         when I_MFPS =>
            Result (This.Get_PS, Set_Flags => False);
      end case;
      This.Current_Timing := This.Current_Timing + Timing;
   end Single_Operand;

   -----------
   -- Start --
   -----------

   procedure Start
     (This : in out Instance'Class)
   is
   begin
      Trace_Execution := Pdp11.Options.Trace;
      This.Start_Time := Ada.Calendar.Clock;
      This.Start_Clock := This.Clock;
      Limit_Speed := Pdp11.Options.Limit_Speed;
      loop
         This.Execute_Next_Instruction;
      end loop;
   end Start;

   --------------------------------
   -- Update_Float_Operand_Value --
   --------------------------------

   procedure Update_Float_Operand_Value
     (This : in out Instance'Class;
      Operand : Pdp11.ISA.Operand_Type;
      Update  : not null access
        procedure (X : in out Float_32))
   is
      use ISA;
   begin
      if Operand.Mode = Register_Mode
        and then not Operand.Deferred
      then
         Update (This.ACs (FP_Register_Index (Operand.Register)));
      else
         declare
            A : constant Address_Type :=
                  This.Get_Operand_Address (Operand, True);
            X : Float_32;
         begin
            This.Get_Float_32 (A, X);
            Update (X);
            This.Set_Float_32 (A, X);
            if (Operand.Mode = Autoincrement_Mode
                or else Operand.Mode = Autodecrement_Mode)
              and then not Operand.Deferred
            then
               This.Rs (Operand.Register) :=
                 This.Rs (Operand.Register) + 2;
            end if;
         end;
      end if;
   end Update_Float_Operand_Value;

   ---------------------
   -- Vector_Format_1 --
   ---------------------

   procedure Vector_Format_1
     (This     : in out Instance'Class;
      Instruction : ISA.Vector_F1;
      VAC         : ISA.V_Register_Index;
      Operand     : ISA.Operand_Type)
   is
      use ISA;
   begin
      case Instruction is
         when I_LDV =>
            This.VRs (VAC) :=
              This.Get_Vector_Operand_Value (Operand);
         when I_STV =>
            This.Set_Vector_Operand_Value
              (Operand, This.VRs (VAC));
      end case;
   end Vector_Format_1;

   ---------------------
   -- Vector_Format_2 --
   ---------------------

   procedure Vector_Format_2
     (This     : in out Instance'Class;
      Instruction : ISA.Vector_F2;
      VAC_1       : ISA.V_Register_Index;
      VAC_2       : ISA.V_Register_Index)
   is
      use ISA;
      Src : constant Vector_96 := This.VRs (VAC_1);
      Dst : Vector_96 renames This.VRs (VAC_2);
      Src_F : constant Float_32 :=
                This.ACs (FP_Register_Index (VAC_1));
      Dst_F : Float_32 renames
                This.ACs (FP_Register_Index (VAC_2));
   begin
      case Instruction is
         when I_ADDV =>
            Dst.X := Dst.X + Src.X;
            Dst.Y := Dst.Y + Src.Y;
            Dst.Z := Dst.Z + Src.Z;
         when I_SUBV =>
            Dst.X := Dst.X - Src.X;
            Dst.Y := Dst.Y - Src.Y;
            Dst.Z := Dst.Z - Src.Z;
         when I_ABSV =>
            Dst_F :=
              Sqrt (Src.X ** 2 + Src.Y ** 2 + Src.Z ** 2);
         when I_MULFV =>
            Dst.X := Dst.X * Src_F;
            Dst.Y := Dst.Y * Src_F;
            Dst.Z := Dst.Z * Src_F;
      end case;
   end Vector_Format_2;

   ---------------------
   -- Vector_Format_3 --
   ---------------------

   procedure Vector_Format_3
     (This     : in out Instance'Class;
      Instruction : ISA.Vector_F3;
      VAC         : ISA.V_Register_Index)
   is
      use ISA;
      Dst   : Vector_96 renames This.VRs (VAC);
   begin
      case Instruction is
         when I_CLRV =>
            Dst := (0.0, 0.0, 0.0);
         when I_NEGV =>
            Dst := (-Dst.X, -Dst.Y, -Dst.Z);
         when I_NORMV =>
            declare
               D : constant Float_32 :=
                     Sqrt (Dst.X ** 2 + Dst.Y ** 2 + Dst.Z ** 2);
            begin
               Dst := (Dst.X / D, Dst.Y / D, Dst.Z / D);
            end;
      end case;
   end Vector_Format_3;

   --------------------
   -- Write_Register --
   --------------------

   procedure Write_Register
     (This    : in out Instance'Class;
      Register : Pdp11.ISA.Register_Index;
      Word     : Boolean;
      Value    : Word_16)
   is
   begin
      if Word then
         This.Rs (Register) := Value;
      else
         This.Rs (Register) :=
           (if Value mod 256 >= 128
            then 255 * 256
            else 0)
           + (Value and 255);
      end if;
   end Write_Register;

end Pdp11.Machine;
