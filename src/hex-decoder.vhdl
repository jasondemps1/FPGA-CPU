library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity HexDecoder is
  port(
    VAL_IN   : in std_logic_vector(3 downto 0);
    DATA_OUT : out std_logic_vector(0 to 6)
    );
end HexDecoder;

architecture HexDec_Arch of HexDecoder is
  signal temp : std_logic_vector(0 to 6) := (others => '1');
begin
  DATA_OUT <= temp;
  
  process(all)
  begin
    case unsigned(VAL_IN) is
      when X"0" => temp <= "0000001";
      when X"1" => temp <= "1001111";
      when X"2" => temp <= "0010010";
      when X"3" => temp <= "0000110";
      when X"4" => temp <= "1001100";
      when X"5" => temp <= "0100100";
      when X"6" => temp <= "0100000";
      when X"7" => temp <= "0001111";
      when X"8" => temp <= "0000000";
      when X"9" => temp <= "0001100";
      when X"A" => temp <= "0001000";
      when X"B" => temp <= "1100000";
      when X"C" => temp <= "0110001";
      when X"D" => temp <= "1000010";
      when X"E" => temp <= "0110000";
      when X"F" => temp <= "0111000";
      when others => temp <= "1010101";
    end case;
  end process;        

end HexDec_Arch;
  
