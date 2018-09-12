getBmi :: (RealFloat a) => a -> a -> a
getBmi weight height = weight / height ^ 2

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  | bmi <= 18.5 = "You're underweight, you emo, yo!"
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly"
  | bmi <= 30.0 = "You're fact! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
-- like if () { } else if () {} else if () {} else {}

getBmiTell :: (RealFloat a) => a -> a -> String
getBmiTell weight height = bmiTell (getBmi weight height)