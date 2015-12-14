library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

-- TODO:
--      * LDM/STM
--      * Test UART
--      * if-then-else

entity top_synth is
  port(
    CLOCK_50 : in  std_logic;
    HEX3     : out std_logic_vector(0 to 6);
    HEX2     : out std_logic_vector(0 to 6);
    HEX1     : out std_logic_vector(0 to 6);
    HEX0     : out std_logic_vector(0 to 6);
    SW       : in  std_logic_vector(9 downto 0);
    LEDR     : out std_logic_vector(9 downto 0);
    LEDG     : out std_logic_vector(7 downto 0);
    KEY      : in  std_logic_vector(3 downto 0);
    GPIO_0   : out std_logic_vector(7 downto 0)
    );
end top_synth;

architecture top_synth of top_synth is
  component imem is
    port(im_addr  : in  unsigned(10 downto 1);
         im_instr : out unsigned(15 downto 0);
         clock    : in  std_logic);
  end component;
  component dmem is
    port(dm_addr     : in  unsigned(10 downto 2);
         dm_data_wr  : in  unsigned(31 downto 0);
         dm_data_rd  : out unsigned(31 downto 0);
         dm_we       : in  std_logic;
         clock       : in  std_logic;
         dm_data_out : out unsigned(31 downto 0)
         );
  end component;
  component rfile is
    port(radr1, radr2, radr3, wadr : in  unsigned(3 downto 0);
         wdata                     : in  unsigned(31 downto 0);
         rdata1, rdata2, rdata3    : out unsigned(31 downto 0);
         write                     : in  std_logic;
         clock                     : in  std_logic);
  end component;
  component Pipeline is
    port(
      clock                     : in     std_logic;
      im_instr                  : in     unsigned(15 downto 0);
      im_addr                   : buffer unsigned(31 downto 1);
      dm_data_rd                : in     unsigned(31 downto 0);
      dm_addr                   : out    unsigned(31 downto 2);
      dm_data_wr                : out    unsigned(31 downto 0);
      dm_we                     : out    std_logic;
      radr1, radr2, radr3, wadr : buffer unsigned(3 downto 0);
      wdata                     : buffer unsigned(31 downto 0);
      rdata1, rdata2, rdata3    : in     unsigned(31 downto 0);
      rf_wr                     : out    std_logic;
      pc_out                    : out    unsigned(31 downto 1);
      branch_val_out            : out    signed(11 downto 1);
      instr_exec_out            : buffer unsigned(15 downto 0)
      );
  end component;
  component HexDecoder is
    port(
      VAL_IN   : in  std_logic_vector(3 downto 0);
      DATA_OUT : out std_logic_vector(0 to 6)
      );
  end component HexDecoder;
  component Interceptor is
    generic(
      N          : natural;
      lookup_tbl : lookup_t(N-1 downto 0)
      );
    port(
      addr     : in  unsigned(10 downto 2);
      data     : in  unsigned(31 downto 0);
      data_out : out unsigned(31 downto 0)
      );
  end component;
  component clock_div is
    generic (N : natural);
    port(
      halt    : buffer std_logic;
      rst     : in     std_logic;
      clk_in  : in     std_logic;
      clk_out : buffer std_logic
      );
  end component;
  component UART_Control is
    generic (WORD_SZ : natural);
    port(
      clock    : in  std_logic;
      word     : in  unsigned(WORD_SZ-1 downto 0);
      data_out : out std_logic_vector(7 downto 0)
      );
  end component;

  signal im_addr                              : unsigned(31 downto 1);
  signal im_instr                             : unsigned(15 downto 0);
  signal dm_addr                              : unsigned(31 downto 2);
  signal dm_data_wr                           : unsigned(31 downto 0);
  signal dm_data_rd                           : unsigned(31 downto 0);
  signal dm_data_out                          : unsigned(31 downto 0);
  signal dm_we                                : std_logic;
  signal radr1, radr1_tmp, radr2, radr3, wadr : unsigned(3 downto 0);
  signal wdata, rdata1, rdata2, rdata3        : unsigned(31 downto 0);
  signal rf_wr                                : std_logic;
  signal clock, clock_tmp                     : std_logic := '1';
  signal branch_value                         : signed(11 downto 1);

  signal transmit : std_logic := '0';

  signal instr_exec : unsigned(15 downto 0);

  signal pc : unsigned(31 downto 1);

  signal intercept_out : unsigned(31 downto 0);

  constant div_amt : natural := 5000000;
  constant addr_lookup : lookup_t := (  -- TODO: Trying to initialize a constant 2d array..
    3, others => '0'
    );

  signal halt : std_logic;

-- Hex outs
  signal HexOut0, HexOut1, HexOut2, HexOut3 : std_logic_vector(0 to 6);

begin
  HEX3 <= HexOut3;
  HEX2 <= HexOut2;
  HEX1 <= HexOut1;
  HEX0 <= HexOut0;

  cpu_pipeline : Pipeline port map(clock, im_instr, im_addr, dm_data_rd, dm_addr, dm_data_wr, dm_we, radr1_tmp, radr2, radr3, wadr, wdata, rdata1, rdata2, rdata3, rf_wr, pc, branch_value, instr_exec);

  clk_div : clock_div
    generic map(div_amt)
    port map(halt, not(KEY(0)), CLOCK_50, clock_tmp);

  --with halt select radr1 <=
  --  unsigned(SW(3 downto 0)) when '1',
  --  radr1_tmp                when others;

  radr1 <= radr1_tmp;

  with halt select LEDR(9 downto 0) <=
    std_logic_vector(rdata1(9 downto 0))        when '1',
    std_logic_vector(intercept_out(9 downto 0)) when others;

--  with halt select clock <=
--    not(KEY(1)) when '1',
--    clock_tmp   when others;

  with not(KEY(1)) select clock <=
    clock_tmp when '0',
    CLOCK_50  when others;

  LEDG(7 downto 0) <= std_logic_vector(instr_exec(7 downto 0));

  data_mem  : dmem port map(dm_addr(10 downto 2), dm_data_wr, dm_data_rd, dm_we, clock, dm_data_out);
  prog_mem  : imem port map(im_addr(10 downto 1), im_instr, clock);
  rfile_mem : rfile port map(radr1, radr2, radr3, wadr, wdata, rdata1, rdata2, rdata3, rf_wr, clock);

  intercept : Interceptor generic map(addr_lookup) port map(dm_addr(10 downto 2), dm_data_out, intercept_out);

  hex0_comp : HexDecoder port map(std_logic_vector(intercept_out(3 downto 0)), HexOut0);
  hex1_comp : HexDecoder port map(std_logic_vector(intercept_out(7 downto 4)), HexOut1);
  hex2_comp : HexDecoder port map(std_logic_vector(intercept_out(11 downto 8)), HexOut2);
  hex3_comp : HexDecoder port map(std_logic_vector(intercept_out(15 downto 12)), HexOut3);

  UART : UART_Control generic map(32) port map(CLOCK_50, dm_data_out, GPIO_0(0));

end top_synth;
