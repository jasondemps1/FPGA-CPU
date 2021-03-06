library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

use work.helpers.all;

-- TODO:
--      * Implement LDM/STM

entity Decode is
  port(
    clock               : in     std_logic;
    stall               : buffer std_logic := '0';
    im_instr            : in     unsigned(15 downto 0);
    radr1, radr2, radr3 : out    unsigned(3 downto 0);
    instr               : out    unsigned(15 downto 0)
    );
end Decode;

architecture Decode of Decode is
  type stall_state_t is (Off, Init, Done);

  signal stall_state : stall_state_t := Off;
  signal stall_instr : unsigned(15 downto 0);
begin

  process(im_instr, stall_state)

  begin
    stall <= '0';
    radr1 <= (others => '0');
    radr2 <= (others => '0');
    radr3 <= (others => '0');

    -- State machine
    case stall_state is
      when Off =>
        -- Detect stall here
        case? im_instr(15 downto 9) is
          -- LOAD MNEMONICS
          -- We need to specify each, so we can stall.
          when "01011--" | "0101011" | "01101--" | "01111--" | "10001--" | "01001--" | "10011--" =>
            stall <= '1';

          when others =>
            stall <= '0';
        end case?;

        radr1 <= Register_Format(im_instr(2 downto 0));
        radr2 <= Register_Format(im_instr(5 downto 3));
        radr3 <= Register_Format(im_instr(8 downto 6));

        case? im_instr(15 downto 9) is
          -- Rdn op Imm8 Mnemonics
          when "001----" | "1010---" | "1100---" =>
            radr3 <= Register_Format(im_instr(11 downto 8));

          -- 4-bit Register Mnemonics
          when "010001-" =>
            radr1 <= Register_Format(im_instr(7) & im_instr(2 downto 0));
            radr2 <= Register_Format(im_instr(6 downto 3));

          when "01001--" | "10011--" =>  -- LDR (Literal)
            radr3 <= Register_Format(im_instr(10 downto 8));

          when others =>
            radr1 <= Register_Format(im_instr(2 downto 0));
        end case?;

      when Init =>
        --stall_instr <= im_instr;
        -- Possibly record pc here somehow and restore?


      when Done =>
        stall <= '0';

    end case;

  end process;


  process (clock)

  begin
    -- Progress the state machine
    if rising_edge(clock) then
      instr <= im_instr;

      if stall then
        instr <= NOP;

        case stall_state is
          when Off =>
            stall_state <= Init;
          when Init =>
            stall_instr <= im_instr;
            stall_state <= Done;
          when Done =>
            stall_state <= Off;
            instr       <= stall_instr;
        end case;
      else
        stall_state <= Off;
      end if;
    end if;
  end process;

end Decode;
