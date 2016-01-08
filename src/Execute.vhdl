library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;

use work.helpers.all;
use work.instr_set.all;

entity Execute is
  port(
    clock                      : in     std_logic;
    instr                      : in     unsigned(15 downto 0);
    rdata1, rdata2, rdata3     : in     unsigned(31 downto 0);
    pc                         : in     unsigned(31 downto 1);
    defer_load                 : out    std_logic;
    branch_value               : out    signed(10 downto 0);
    Nflag, Zflag, Cflag, Vflag : buffer std_logic;
    wadr                       : out    unsigned(3 downto 0);
    wdata                      : out    unsigned(31 downto 0);
    rf_wr                      : out    std_logic;
    dm_addr                    : out    unsigned(31 downto 2);
    dm_data_wr                 : out    unsigned(31 downto 0);
    dm_we                      : out    std_logic
    );
end Execute;

architecture Execute of Execute is
  component comb_mult is
    port
      (
        dataa  : in  std_logic_vector (31 downto 0);
        datab  : in  std_logic_vector (31 downto 0);
        result : out std_logic_vector (31 downto 0)
        );
  end component;

  signal Ztmp, Ctmp, Vtmp, Ntmp : std_logic;

  signal wadr_tmp       : unsigned(3 downto 0);
  signal wdata_tmp      : unsigned(31 downto 0);
  signal rf_wr_tmp      : std_logic;
  signal dm_addr_tmp    : unsigned(31 downto 2);
  signal dm_data_wr_tmp : unsigned(31 downto 0);
  signal dm_we_tmp      : std_logic;
  signal defer_tmp      : std_logic;

  signal multa   : std_logic_vector(31 downto 0);
  signal multb   : std_logic_vector(31 downto 0);
  signal multres : std_logic_vector(31 downto 0);

  attribute multstyle            : string;
  attribute multstyle of Execute : architecture is "dsp";
begin

  mult : comb_mult port map (multa, multb, multres);

  --wadr <= wadr_tmp;

  --wdata <= wdata_tmp;
  --rf_wr <= rf_wr_tmp;

  --dm_addr    <= dm_addr_tmp;
  --dm_data_wr <= dm_data_wr_tmp;
  --dm_we      <= dm_we_tmp;

  --defer_load <= defer_tmp;

  process (instr, rdata1, rdata2, rdata3, pc, Nflag, Zflag, Cflag, Vflag, multres)
    variable condition   : unsigned(3 downto 0);
    variable take_branch : boolean;
    variable sum         : unsigned(32 downto 0);
    variable flags       : flags_bv;

    variable wadr_lcl  : unsigned(3 downto 0)  := (others => '0');
    variable wdata_lcl : unsigned(31 downto 0) := (others => '0');
    variable rf_wr_lcl : std_logic             := '0';

    variable dm_addr_lcl    : unsigned(31 downto 2) := (others => '0');
    variable dm_data_wr_lcl : unsigned(31 downto 0) := (others => '0');
    variable dm_we_lcl      : std_logic             := '0';

    variable defer_lcl : std_logic;

    variable branch_tmp : signed(10 downto 0);

    --variable pc_lcl : unsigned(31 downto 1);

  begin
    --rf_wr        <= '0';
    branch_tmp := "00000000001";

    --defer_load <= '0';

    take_branch := false;

    Ztmp <= '0';
    Ntmp <= '0';
    Ctmp <= '0';
    Vtmp <= '0';

    wadr_lcl       := (others => '0');
    wdata_lcl      := (others => '0');
    rf_wr_lcl      := '0';
    dm_addr_lcl    := (others => '0');
    dm_data_wr_lcl := (others => '0');
    dm_we_lcl      := '0';
    defer_lcl      := '0';
    multa          <= (others => '0');
    multb          <= (others => '0');
    --pc_lcl                     := pc;

    if instr /= NOP then
      if xor_reduce(std_logic_vector(instr)) /= 'U' and xor_reduce(std_logic_vector(instr)) /= 'X' then
        -- Deduce type of instruction
        -- Feed to that function
        -- Further deduction required within function
        -- Return flags if necessary.

        flags := Nflag & Zflag & Cflag & Vflag;

        if instr(15 downto 6) = "0100001101" then
          multa <= std_logic_vector(rdata1);
          multb <= std_logic_vector(rdata2);
        end if;

        case instr(15 downto 13) is
          when "000" =>
            alu(instr, flags, rdata1, rdata2, rdata3, wadr_lcl, wdata_lcl, rf_wr_lcl, flags, multres);

          when "001" =>                 -- Move/Compare/add/sub #Imm8
            alu_imm8(instr, flags, rdata1, rdata2, rdata3, wadr_lcl, wdata_lcl, rf_wr_lcl, flags);

          when "010" =>
            if instr(12) = '0' then
              if instr(11) = '1' then   -- PC-relative load.
                pc_rel_load(instr, pc, rdata2, rdata3, dm_addr_lcl, dm_we_lcl, defer_lcl);
              end if;

              if instr(11 downto 10) = "00" then     -- ALU operations
                alu(instr, flags, rdata1, rdata2, rdata3, wadr_lcl, wdata_lcl, rf_wr_lcl, flags, multres);
              elsif instr(11 downto 10) = "01" then  -- hi reg operations / branch exchange
                hi_reg_bx(instr, flags, rdata1, rdata2, pc, wadr_lcl, wdata_lcl, rf_wr_lcl, flags);
              end if;


            else
              if instr(9) = '0' then    -- Load/store register offset
                load_store_offset(instr, rdata1, rdata2, rdata3, dm_addr_lcl, dm_we_lcl, dm_data_wr_lcl, defer_lcl);
              else                      --Load/store sign-extend byte/halfword
                load_store_sign(instr, rdata1, rdata2, rdata3, dm_addr_lcl, dm_we_lcl, dm_data_wr_lcl, defer_lcl);
              end if;
            end if;

          when "011" | "100" =>
            -- Load/store immediate offset or l/s halfword
            if instr(12) = '0' then
              load_store_imm(instr, rdata1, rdata2, rdata3, dm_addr_lcl, dm_we_lcl, dm_data_wr_lcl, defer_lcl);
            else                        --SP relative load/store
              load_store_sp(instr, rdata3, dm_addr_lcl, dm_we_lcl, dm_data_wr_lcl, defer_lcl);
            end if;

            --when "100" =>
            --  if instr(12) = '0' then
            --    -- load/store halfword
            --    else
            --  -- SP-relative load/store
            --  end if;

          when "101" =>
            if instr(12) = '0' then     -- Load address
              load_address(instr, pc, wadr_lcl, wdata_lcl, rf_wr_lcl, flags);
            elsif instr(12) = '1' then

--misc_instr(instr, rdata1, rdata2, wadr_lcl, wdata_lcl, rf_wr_lcl, flags);
              if instr(11 downto 10) = "00" then  -- TODO: This is wrong....
                if instr(8) = '1' then
                  -- CBNZ/CBZ
                  --take_branch := comp_branch(instr, rdata1, flags);

                  if take_branch then
                    branch_tmp := resize(signed(instr(7 downto 3)), 11);
                  end if;
                else
                  offset_sp_sign(instr, rdata1, rdata2, wadr_lcl, wdata_lcl, rf_wr_lcl, flags);
                end if;
              end if;
            --else
            ----Push/pop regs
            --end if;
            end if;

          when "110" =>
            if instr(12) = '0' then
              -- Multiple load/store
              else
              if instr(11 downto 8) = "1111" then
                -- Software Interrupt
                else
                  -- Conditional Branch
                  take_branch := branch(instr, flags);

                if take_branch then
                  branch_tmp := resize(signed(instr(7 downto 0)), 11);
                end if;

              end if;
            end if;

          when "111" =>
            if instr(12) = '0' then
              -- Unconditional Branch
              branch_tmp := signed(instr(10 downto 0));
            else
            -- Long branch w/ Link
            end if;

          when others =>
            branch_tmp := (others => '0');
        end case;

        Ntmp <= flags(3);
        Ztmp <= flags(2);
        Ctmp <= flags(1);
        Vtmp <= flags(0);

      end if;
    end if;

    wadr  <= wadr_lcl;
    wdata <= wdata_lcl;
    rf_wr <= rf_wr_lcl;

    dm_addr    <= dm_addr_lcl;
    dm_data_wr <= dm_data_wr_lcl;
    dm_we      <= dm_we_lcl;

    defer_load <= defer_lcl;

    branch_value <= branch_tmp;
  end process;


  -- store flags
  process (clock)
  begin
    if rising_edge(clock) then
      Zflag <= Ztmp;
      Cflag <= Ctmp;
      Nflag <= Ntmp;
      Vflag <= Vtmp;
    end if;
  end process;

end Execute;
