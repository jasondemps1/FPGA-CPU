library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;

use work.helpers.all;

-- Pitfalls:
-- * Flags might not be set correctly.
-- * Verify flags on functions

package instr_set is
  type flags_bv is array(3 downto 0) of std_logic;  -- NZCV
  type instr_arr is array(15 downto 11) of std_logic;
  
  -- Idea: Define all instructions we need to hook (extra func.) for store.
  -- Can't use first 5 bits of instruct. though.......
  --constant Store_Hooks : instr_arr := (
  --  "01010", -- LDRSB
  --  "01011",
  --  "01011", -- LDRB
  --  "",

  --  );

--  component test_mult IS
--      PORT
--      (
--              clock0          : IN STD_LOGIC  := '1';
--              dataa           : IN STD_LOGIC_VECTOR (31 DOWNTO 0) :=  (OTHERS => '0');
--              datab           : IN STD_LOGIC_VECTOR (31 DOWNTO 0) :=  (OTHERS => '0');
--              overflow                : OUT STD_LOGIC ;
--              result          : OUT STD_LOGIC_VECTOR (31 DOWNTO 0)
--      );
--      END component;

  function branch(
    instr : unsigned(15 downto 0);
    flags : flags_bv
    ) return boolean;                   -- Return: take branch or not.

  function comp_branch(
    instr  : unsigned(15 downto 0);
    rdata1 : unsigned(31 downto 0);
    flags  : flags_bv
    ) return boolean;                   -- Return: take branch or not.

  procedure alu(
    instr                  :     unsigned(15 downto 0);
    flags                  :     flags_bv;
    rdata1, rdata2, rdata3 :     unsigned(31 downto 0);
    wadr                   : out unsigned(3 downto 0);
    wdata                  : out unsigned(31 downto 0);
    wr_reg                 : out std_logic;
    flags_out              : out flags_bv;
--	 multa : out std_logic_vector(31 downto 0);
--	 multb : out std_logic_vector(31 downto 0);
	 multres : std_logic_vector(31 downto 0)
    );


  procedure alu_imm8(
    instr                  :     unsigned(15 downto 0);
    flags                  :     flags_bv;
    rdata1, rdata2, rdata3 :     unsigned(31 downto 0);
    wadr                   : out unsigned(3 downto 0);
    wdata                  : out unsigned(31 downto 0);
    wr_reg                 : out std_logic;
    flags_out              : out flags_bv
    );

  procedure hi_reg_bx(
    instr          :     unsigned(15 downto 0);
    flags          :     flags_bv;
    rdata1, rdata2 :     unsigned(31 downto 0);
    pc             : in  unsigned(31 downto 1);
    wadr           : out unsigned(3 downto 0);
    wdata          : out unsigned(31 downto 0);
    wr_reg         : out std_logic;
    flags_out      : out flags_bv
    );

  procedure pc_rel_load(
    instr          :     unsigned(15 downto 0);
    pc             :     unsigned(31 downto 1);
    rdata2, rdata3 :     unsigned(31 downto 0);
    dm_addr        : out unsigned(31 downto 2);
    dm_we          : out std_logic;
    stall          : out std_logic
    );

  procedure load_store_offset(
    instr                  :     unsigned(15 downto 0);
    rdata1, rdata2, rdata3 :     unsigned(31 downto 0);
    dm_addr                : out unsigned(31 downto 2);
    dm_we                  : out std_logic;
    dm_data_wr             : out unsigned(31 downto 0);
    stall                  : out std_logic
    );

  procedure load_store_sign(
    instr                  :     unsigned(15 downto 0);
    rdata1, rdata2, rdata3 :     unsigned(31 downto 0);
    dm_addr                : out unsigned(31 downto 2);
    dm_we                  : out std_logic;
    dm_data_wr             : out unsigned(31 downto 0);
    stall                  : out std_logic
    );

  procedure load_store_imm(
    instr                  :     unsigned(15 downto 0);
    rdata1, rdata2, rdata3 :     unsigned(31 downto 0);
    dm_addr                : out unsigned(31 downto 2);
    dm_we                  : out std_logic;
    dm_data_wr             : out unsigned(31 downto 0);
    stall                  : out std_logic
    );

  procedure load_store_sp(
    instr      :     unsigned(15 downto 0);
    rdata3     :     unsigned(31 downto 0);
    dm_addr    : out unsigned(31 downto 2);
    dm_we      : out std_logic;
    dm_data_wr : out unsigned(31 downto 0);
    stall      : out std_logic
    );

  procedure load_address(
    instr  :     unsigned(15 downto 0);
    pc     :     unsigned(31 downto 1);
    wadr   : out unsigned(3 downto 0);
    wdata  : out unsigned(31 downto 0);
    wr_reg : out std_logic;
    flags  : out flags_bv
    );

  procedure offset_sp_sign(
    instr          :     unsigned(15 downto 0);
    rdata1, rdata2 :     unsigned(31 downto 0);
    wadr           : out unsigned(3 downto 0);
    wdata          : out unsigned(31 downto 0);
    wr_reg         : out std_logic;
    flags          : out flags_bv
    );

  function flags_set(
    value : unsigned(32 downto 0);
    mask  : flags_bv
    ) return flags_bv;

  procedure misc_instr(
    instr          :     unsigned(15 downto 0);
    rdata1, rdata2 :     unsigned(31 downto 0);
    wadr           : out unsigned(3 downto 0);
    wdata          : out unsigned(31 downto 0);
    wr_reg         : out std_logic;
    flags          : out flags_bv
    );

end package;

-- BEGIN BODY -- 


package body instr_set is

  function branch(
    instr : unsigned(15 downto 0);
    flags : flags_bv
    ) return boolean is

    variable take_branch : boolean   := false;
    variable Nflag       : std_logic := flags(3);
    variable Zflag       : std_logic := flags(2);
    variable Cflag       : std_logic := flags(1);
    variable Vflag       : std_logic := flags(0);
  begin
    Nflag := flags(3);
    Zflag := flags(2);
    Cflag := flags(1);
    Vflag := flags(0);

    if instr(15 downto 12) = "1110" then
      return true;
    elsif instr(15 downto 12) = "1101" then
      -- B<C> offset
      case instr(11 downto 9) is
        when "000" => take_branch := (Zflag = '1');
        when "001" => take_branch := (Cflag = '1');
        when "010" => take_branch := (Nflag = '1');
        when "011" => take_branch := (Vflag = '1');
        when "100" => take_branch := (Cflag = '1' and Zflag = '0');
        when "101" => take_branch := (Nflag = Vflag);
        when "110" => take_branch := (Zflag = '0' and Nflag = Vflag);
        when "111" => take_branch := true;

        when others => take_branch := false;
                       report "Invalid condition code in Branch instruction" severity error;
      end case;

      if instr(8) = '1' then
        -- inverted condition
        take_branch := not take_branch;
      end if;
    end if;

    return take_branch;

  end branch;

  --procedure branc_exec(
  --  instr       :     unsigned(15 downto 0);
  --  flags       :     flags_bv;
  --  pc          : out unsigned(31 downto 1);
  --  take_branch : out boolean;
  --  ) is

  --  variable branch_val : unsigned(3 downto 0) := instr(6 downto 3);
  --begin
  --  pc := branch_val;
  --  branch(branch_val, flags);
  --end branch_exec;

  function comp_branch(
    instr  : unsigned(15 downto 0);
    rdata1 : unsigned(31 downto 0);
    flags  : flags_bv
    ) return boolean is

  begin
    if (instr(11) = '1') xor (rdata1 = (others => '0')) then
      return true;
    else
      return false;
    end if;

  end function;


  procedure alu(
    instr                  :     unsigned(15 downto 0);
    flags                  :     flags_bv;
    rdata1, rdata2, rdata3 :     unsigned(31 downto 0);
    wadr                   : out unsigned(3 downto 0);
    wdata                  : out unsigned(31 downto 0);
    wr_reg                 : out std_logic;
    flags_out              : out flags_bv;
--	 multa : out std_logic_vector(31 downto 0);
--	 multb : out std_logic_vector(31 downto 0);
	 multres : std_logic_vector(31 downto 0)
    ) is

    variable res        : unsigned(32 downto 0);
    variable apply_val  : std_logic := '1';  -- do we apply the value?
    variable flags_mask : flags_bv  := "1111";              -- NZCV
  begin
    if instr(15 downto 13) = "000" then
      case instr(12 downto 11) is
        when "00" =>                    -- MOV / LSL #Imm
          res        := resize(rdata2 sll to_integer(instr(10 downto 6)), 33);
          flags_mask := "1100";
        when "01" =>                    -- LSR #Imm
          res        := resize(rdata2 srl to_integer(instr(10 downto 6)), 33);
          flags_mask := "1110";
        when "10" =>                    --ASR #Imm
          res        := resize(unsigned(signed(rdata2) srl to_integer(instr(10 downto 6))), 33);
          flags_mask := "1110";
        when "11" =>                    -- ADD/SUB
          case instr(10 downto 9) is
            when "00" =>                -- Add (Reg)
              res := resize(rdata2 + rdata3, 33);
            when "01" =>                -- Sub (Reg)
              res := resize(rdata2 - rdata3, 33);
            when "10" =>                -- Add #Imm3
              res := resize(rdata2 + instr(8 downto 6), 33);
            when "11" =>                -- Sub #Imm3
              res := resize(rdata2 - instr(8 downto 6), 33);
            when others =>
              res := (others => 'U');
          end case;
        when others =>
          res := (others => 'U');
      end case;
    else
      case instr(10 downto 6) is
        when "00000" =>                 -- AND
          res        := resize(rdata1 and rdata2, 33);
          flags_mask := "1110";
        when "00001" =>                 -- EOR
          res        := resize(rdata1 xor rdata2, 33);
          flags_mask := "1110";
        when "00010" =>                 -- LSL
          res        := resize(rdata1 sll to_integer(rdata2 and resize(X"1F", rdata2'length)), 33);
          flags_mask := "1110";
        when "00011" =>                 -- LSR
          res        := resize(rdata1 srl to_integer(rdata2 and resize(X"1F", rdata2'length)), 33);
          flags_mask := "1110";
        when "00100" =>                 -- ASR
          res        := resize(unsigned(signed(rdata1) srl to_integer(rdata2 and resize(X"1F", rdata2'length))), 33);
          flags_mask := "1110";
        when "00101" =>                 -- ADC
          res := resize(rdata1 + ("0" & flags(2)), 33);
        when "00110" =>                 -- SBC
          res := resize(rdata1 - (rdata2 - not ("0" & flags(2))), 33);
        when "00111" =>                 -- ROR
          res        := resize(rdata1 ror to_integer(rdata2 and resize(X"1F", rdata2'length)), 33);
          flags_mask := "1110";
        when "01000" =>                 -- TST
          apply_val  := '0';
          res        := resize(rdata1 and rdata2, 33);
          flags_mask := "1110";
        when "01001" =>                 -- RSB/NEG
          res := resize(unsigned(std_logic_vector(-signed(rdata2))), 33);
        when "01010" =>                 -- CMP
          apply_val := '0';
          res       := resize(rdata1 - rdata2, 33);
        when "01011" =>                 -- CMN
          apply_val := '0';
          res       := resize(rdata1 + rdata2, 33);
        when "01100" =>                 -- ORR
          res        := resize(rdata1 or rdata2, 33);
          flags_mask := "1110";
        when "01101" =>                 -- MUL
		    --multa := std_logic_vector(rdata1);
			 --multb := std_logic_vector(rdata2);
			 --res := unsigned(multres);
          res := "0" & unsigned(multres); --"0" & resize(rdata1 * rdata2, 32);  --resize(rdata1 * rdata2, 33);
          flags_mask := "1100";
        when "01110" =>                 -- BIC
          res        := resize(rdata1 and not rdata2, 33);
          flags_mask := "1110";
        when "01111" =>                 -- MVN
          res        := resize(unsigned(std_logic_vector(-signed(rdata2))), 33);
          flags_mask := "1110";
        when others =>
          res := (others => 'U');
      end case;
    end if;

    if apply_val = '1' then
      wadr   := "0" & instr(2 downto 0);
      wdata  := res(31 downto 0);
      wr_reg := '1';
    end if;

    flags_out := flags_set(res, flags_mask);

  end alu;

  procedure alu_imm8(
    instr                  :     unsigned(15 downto 0);
    flags                  :     flags_bv;
    rdata1, rdata2, rdata3 :     unsigned(31 downto 0);
    wadr                   : out unsigned(3 downto 0);
    wdata                  : out unsigned(31 downto 0);
    wr_reg                 : out std_logic;
    flags_out              : out flags_bv
    ) is

    variable res        : unsigned(32 downto 0);
    variable imm8       : unsigned(7 downto 0);
    variable flags_mask : flags_bv := "1111";
  begin
    imm8 := instr(7 downto 0);

    case instr(12 downto 11) is
      when "00" =>                      -- MOV #Imm8
        res        := resize(imm8, 33);
        flags_mask := "1110";
      when "01" =>                      -- CMP #Imm8
        res := resize(rdata3 - imm8, 33);
      when "10" =>                      -- Add #Imm8
        res := resize(rdata3 + imm8, 33);
      when "11" =>                      -- Sub #Imm8
        res := resize(rdata3 - imm8, 33);
      when others =>
        res := (others => 'U');
    end case;

    wadr   := "0" & instr(10 downto 8);
    wdata  := res(31 downto 0);
    wr_reg := '1';

    flags_out := flags_set(res, flags_mask);

  end alu_imm8;


  procedure pc_rel_load(
    instr          :     unsigned(15 downto 0);
    pc             :     unsigned(31 downto 1);
    rdata2, rdata3 :     unsigned(31 downto 0);
    dm_addr        : out unsigned(31 downto 2);
    dm_we          : out std_logic;
    stall          : out std_logic
    ) is

  begin
    dm_addr := resize(pc + (instr(7 downto 0) & "0"), 30);
    dm_we   := '0';
    stall   := '1';
  end pc_rel_load;

  procedure load_store_offset(
    instr                  :     unsigned(15 downto 0);
    rdata1, rdata2, rdata3 :     unsigned(31 downto 0);
    dm_addr                : out unsigned(31 downto 2);
    dm_we                  : out std_logic;
    dm_data_wr             : out unsigned(31 downto 0);
    stall                  : out std_logic
    ) is

  begin
    stall := '0';

    case instr(11 downto 10) is
      when "00" =>                      -- STR
        dm_addr    := resize(rdata2 + rdata3, dm_addr'length);
        dm_we      := '1';
        dm_data_wr := rdata1;
      when "01" =>                      -- STRB
        dm_addr    := resize(rdata2 + rdata3, dm_addr'length);
        dm_we      := '1';
        dm_data_wr := rdata1 and (resize("0", 24) & X"FF");
      when "10" | "11" =>               -- LDR | LDRB (Handled on Store)
        dm_addr := resize(rdata2 + rdata3, dm_addr'length);
        dm_we   := '0';
        stall   := '1';
      when others =>
        dm_addr := (others => 'U');
    end case;

  end load_store_offset;

  procedure load_store_sign(
    instr                  :     unsigned(15 downto 0);
    rdata1, rdata2, rdata3 :     unsigned(31 downto 0);
    dm_addr                : out unsigned(31 downto 2);
    dm_we                  : out std_logic;
    dm_data_wr             : out unsigned(31 downto 0);
    stall                  : out std_logic
    ) is

  begin
    stall := '0';

    case instr(11 downto 10) is
      when "00" =>                      -- STRH
        dm_addr    := resize(rdata2 + rdata3, 30);
        dm_we      := '1';
        dm_data_wr := rdata1 and resize(X"FF", rdata1'length);
      when "01" | "10" | "11" =>  -- LDRSB | LDRH | LDRSH (Handled on Store)
        dm_addr := resize(rdata2 + rdata3, 30);
        dm_we   := '1';
        stall   := '1';
      when others =>
        dm_addr := (others => 'U');
    end case;

  end load_store_sign;

  procedure load_store_imm(
    instr                  :     unsigned(15 downto 0);
    rdata1, rdata2, rdata3 :     unsigned(31 downto 0);
    dm_addr                : out unsigned(31 downto 2);
    dm_we                  : out std_logic;
    dm_data_wr             : out unsigned(31 downto 0);
    stall                  : out std_logic
    ) is

    variable imm5 : unsigned(4 downto 0);  -- := instr(10 downto 6);
  begin
    stall := '0';
    imm5  := instr(10 downto 6);

    case instr(12 downto 11) is
      when "00" =>                      -- STR
        dm_addr := resize(rdata2 + (imm5 & "0"), 30);
        dm_we   := '1';
        if instr(15 downto 13) = "100" then  -- STRH
          dm_data_wr := rdata1 and resize(X"FFFF", rdata1'length);
        else
          dm_data_wr := rdata1;
        end if;
      when "01" =>  -- LDR | LDRB (Handle on Store [Also halfword!])
        dm_addr := resize(rdata2 + (imm5 & "0"), 30);
        dm_we   := '0';
        stall   := '1';
      when "10" =>                      -- STRB
        dm_addr    := resize(rdata2 + resize(imm5, 30), 30);
        dm_we      := '1';
        dm_data_wr := rdata1 and resize(X"FF", rdata1'length);
      when "11" =>                      -- LDRB
        dm_addr := resize(rdata2 + resize(imm5, 30), 30);
        dm_we   := '0';
        stall   := '1';
      when others =>
        dm_addr := (others => 'U');
    end case;

  end load_store_imm;


  procedure load_store_sp(
    instr      :     unsigned(15 downto 0);
    rdata3     :     unsigned(31 downto 0);
    dm_addr    : out unsigned(31 downto 2);
    dm_we      : out std_logic;
    dm_data_wr : out unsigned(31 downto 0);
    stall      : out std_logic
    ) is

    variable imm8 : unsigned(7 downto 0);  -- := instr(7 downto 0);
  begin
    imm8 := instr(7 downto 0);
    case instr(11) is
      when '0' =>                          -- STRH
        dm_addr    := resize(SP + (imm8 & "0"), 30);
        dm_we      := '1';
        dm_data_wr := rdata3;
      when '1' =>
        dm_addr := resize(SP + (imm8 & "0"), 30);
      when others =>
        dm_addr := (others => 'U');
    end case;

  end load_store_sp;


  procedure load_address(
    instr  :     unsigned(15 downto 0);
    pc     :     unsigned(31 downto 1);
    wadr   : out unsigned(3 downto 0);
    wdata  : out unsigned(31 downto 0);
    wr_reg : out std_logic;
    flags  : out flags_bv
    ) is

    variable res : unsigned(32 downto 0);
  begin
    if instr(11) = '0' then             -- ADR
      wdata := "0" & pc + instr(7 downto 0);
    else                                -- ADD (SP + #Imm)
      res   := resize(SP + (instr(7 downto 0) & resize("0", 25)), 33);
      wdata := res(31 downto 0);
      flags := flags_set(res, "1111");
    end if;

    wadr   := "0" & instr(10 downto 8);
    wr_reg := '1';

  end load_address;

  procedure offset_sp_sign(
    instr          :     unsigned(15 downto 0);
    rdata1, rdata2 :     unsigned(31 downto 0);
    wadr           : out unsigned(3 downto 0);
    wdata          : out unsigned(31 downto 0);
    wr_reg         : out std_logic;
    flags          : out flags_bv
    ) is

    variable res        : unsigned(32 downto 0);
    variable flags_mask : flags_bv := "0000";
  begin

    --if instr(12 downto 11) = "10" then
    case instr(12 downto 11) is
      when "10" =>
        case instr(9 downto 8) is
          when "00" =>
            wadr   := SP;
            wr_reg := '1';

            if instr(7) = '0' then      -- ADD (SP + #Imm7)
              res        := resize(SP + (instr(6 downto 0) & resize("0", 25)), 33);
              flags_mask := "1111";
            else                        -- SUB (SP - #Imm)
              res        := resize(SP - (instr(6 downto 0) & resize("0", 25)), 33);
              flags_mask := "1111";
            end if;

          when "10" =>                  -- Sign extension
            case instr(7 downto 6) is
              when "00" =>              -- SXTH
                res := unsigned(resize(signed(rdata2 and resize(X"FFFF", rdata2'length)), 33));
              when "01" =>              -- SXTB
                res := unsigned(resize(signed(rdata2 and resize(X"FF", rdata2'length)), 33));
              when "10" =>              -- UXTH
                res := resize(rdata2 and resize(X"FFFF", rdata2'length), 33);
              when "11" =>              -- UXTB
                res := resize(rdata2 and resize(X"FFFF", rdata2'length), 33);
              when others =>
                res := (others => 'U');
            end case;

          when others =>
            res := (others => 'U');
        end case;
      when others =>
        res := (others => 'U');
    end case;
    --else
    --  res := (others => 'U');
    --end if;

    wdata := res(31 downto 0);
    flags := flags_set(res, flags_mask);

  end offset_sp_sign;

  procedure hi_reg_bx(
    instr          :     unsigned(15 downto 0);
    flags          :     flags_bv;
    rdata1, rdata2 :     unsigned(31 downto 0);
    pc             : in  unsigned(31 downto 1);
    wadr           : out unsigned(3 downto 0);
    wdata          : out unsigned(31 downto 0);
    wr_reg         : out std_logic;
    flags_out      : out flags_bv
    ) is

    variable flags_mask : flags_bv := "1111";
  begin
    case instr(9 downto 8) is
      when "00" =>                      -- SP + Reg / 4 bit reg mnemonics
        wadr      := instr(7) & instr(2 downto 0);
        wdata     := resize(rdata1 + rdata2, 32);
        wr_reg    := '1';
        flags_out := flags_set(resize(rdata1 - rdata2, 33), flags_mask);
      when "01" =>                      -- CMP
        flags_out := flags_set(resize(rdata1 - rdata2, 33), flags_mask);
      when "11" =>                      -- BX
      --pc := rdata1;                   -- TODO: Add take_branch here
      --branch(instr, flags_out);
      when others =>
        report "Invalid condition code in High Register instruction" severity error;
    end case;

  end hi_reg_bx;

  procedure misc_instr(
    instr          :     unsigned(15 downto 0);
    rdata1, rdata2 :     unsigned(31 downto 0);
    wadr           : out unsigned(3 downto 0);
    wdata          : out unsigned(31 downto 0);
    wr_reg         : out std_logic;
    flags          : out flags_bv
    ) is

  begin
    if instr(10) = '0' then
      -- REVXX Can probably go here also...?

    --offset_sp_sign(instr, rdata1, rdata2, wadr, wdata, rf_reg, flags);
    else                                -- 10 = '1'
      -- Push, pop, ITE, CPS, etc. goes here.....somewhere.
      --case (instr(9 downto 8)) is

    end if;
    --end case;

  end misc_instr;


  function flags_set(
    value : unsigned(32 downto 0);
    mask  : flags_bv                    -- NZCV update bits
    ) return flags_bv is

    variable flags : flags_bv;
  begin
    flags(0) := value(31);              -- Neg
    flags(2) := value(32);              -- Carry

    -- Check Zero
    if or_reduce(std_logic_vector(value)) = '0' then
      flags(1) := '1';
      flags(3) := value(32) xor '1';
    else
      flags(1) := '0';
      flags(3) := value(32) xor '0';
    end if;

    return
      (flags(3) and mask(3)) &
      (flags(2) and mask(2)) &
      (flags(1) and mask(1)) &
      (flags(0) and mask(0));


  end flags_set;

end instr_set;
