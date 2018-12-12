-- This is for reference, they're defined in Euterpea:
-- https://github.com/Euterpea/Euterpea/blob/6635e483cf80ec8ae67613c40e8d61e475f4742d/Euterpea/Music/Note/Music.hs#L134

type AbsPitch = Int

absPitch           :: Pitch -> AbsPitch
absPitch (pc,oct)  = 12*oct + pcToInt pc

pcToInt     :: PitchClass -> Int
pcToInt pc  = case pc of
  Cff  -> -2;  Cf  -> -1;  C  -> 0;   Cs  -> 1;   Css  -> 2; 
  Dff  -> 0;   Df  -> 1;   D  -> 2;   Ds  -> 3;   Dss  -> 4; 
  Eff  -> 2;   Ef  -> 3;   E  -> 4;   Es  -> 5;   Ess  -> 6; 
  Fff  -> 3;   Ff  -> 4;   F  -> 5;   Fs  -> 6;   Fss  -> 7; 
  Gff  -> 5;   Gf  -> 6;   G  -> 7;   Gs  -> 8;   Gss  -> 9; 
  Aff  -> 7;   Af  -> 8;   A  -> 9;   As  -> 10;  Ass  -> 11;
  Bff  -> 9;   Bf  -> 10;  B  -> 11;  Bs  -> 12;  Bss  -> 13

pitch     :: AbsPitch -> Pitch
pitch ap  = 
    let (oct, n) = divMod ap 12
    in  ([C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B] !! n, oct)

trans      :: Int -> Pitch -> Pitch
trans i p  = pitch (absPitch p + i)
