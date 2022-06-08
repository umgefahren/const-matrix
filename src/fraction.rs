use core::fmt::Display;
use core::num::NonZeroI64;
use core::ops::{Add, Sub, Mul, Div, AddAssign, SubAssign, MulAssign, DivAssign};
use core::str::FromStr;

macro_rules! const_swap {
    ($a:ident, $b:ident) => {
        ($b, $a)
    };
}

macro_rules! fraction_op {
    ($l:ident, $r:ident, $o:tt) => {
        {
            let lcm = lcm_i64($l.den, $r.den);
            let l_m = $l.den / lcm;
            let r_m = $r.den / lcm;
            let l_num = l_m * $l.num;
            let r_num = r_m * $r.num;
            let num = l_num $o r_num;
            Fraction {
                num,
                den: lcm,
            }
        }
    };
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Fraction {
    num: i64,
    den: i64,
}


#[derive(Debug)]
pub enum ParseFractionError {
    NumeratorParseError(core::num::ParseIntError),
    DenominatorParseError(core::num::ParseIntError),
    InvalidFormatting,
}


impl FromStr for Fraction {
    type Err = ParseFractionError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.split('/');
        let num_str = parts.next().ok_or(ParseFractionError::InvalidFormatting)?;
        let num = i64::from_str(num_str).map_err(ParseFractionError::NumeratorParseError)?;
        let den_str = parts.next().ok_or(ParseFractionError::InvalidFormatting)?;
        let den = NonZeroI64::from_str(den_str).map_err(ParseFractionError::DenominatorParseError)?;
        if parts.next().is_some() {
            return Err(ParseFractionError::InvalidFormatting);
        }
        unsafe { Ok(Self::new_unchecked(num, den)) }
    }
}

impl Display for Fraction {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.num.fmt(f)?;
        f.write_str("/")?;
        self.den.fmt(f)
    }
}


impl Fraction {
    /// the one fraction (1/1)
    pub const ONE: Self = Self { num: 1, den: 1 };

    /// the zero fraction (0/1)
    pub const ZERO: Self = Self { num: 0, den: 1 };

    /// construct a new fraction with (`num` / `den`)
    pub fn new(num: impl Into<i64>, den: NonZeroI64) -> Self {
        unsafe { Self::new_unchecked(num, den) }
    }

    /// construct a new fraction with (`num` / `den`)
    ///
    /// # Safety
    ///
    /// creates new fraction without checking wether the denominator is zero.
    pub unsafe fn new_unchecked(num: impl Into<i64>, den: impl Into<i64>) -> Self {
        Self {
            num: num.into(),
            den: den.into(),
        }.shorten()
    }

    /// construct a new fraction with (`num` / `den`)
    ///
    /// # Safety
    /// 
    /// creates new fraction without checking wether the denominator is zero.
    pub const unsafe fn new_const_unchecked(num: i64, den: i64) -> Self {
        Self {
            num,
            den
        }
    }

    /// construct a new fraction with (`int` / 1)
    pub const fn new_int(int: i64) -> Self {
        Self {
            num: int,
            den: 1
        }
    }

    /// converts the fraction to an integer, when the denominator is 1
    pub const fn to_int(self) -> Option<i64> {
        let shortended = self.shorten();
        if shortended.den == 1 {
            return Some(shortended.num);
        }
        None
    }

    pub fn to_float(self) -> f64 {
        let shortened = self.shorten();
        (shortened.num as f64) / (shortened.den as f64)
    }

    /// shortens the fraction
    #[inline]
    pub const fn shorten(mut self) -> Self {
        let cd = gcd_i64(self.num, self.den);
        self.num /= cd;
        self.den /= cd;
        self
    }


    /// inverse the fraction
    #[inline]
    pub const fn inverse(mut self) -> Self {
        (self.num, self.den) = (self.den, self.num);
        self
    }


    /// multiplication that can happen at compile time
    pub const fn const_mul(self, rhs: Self) -> Self {
        let num = self.num * rhs.num;
        let den = self.den * rhs.den;
        let mut ret = Self {
            num,
            den
        };
        ret = ret.shorten();
        ret
    }
   
    /// addition that can happen at compile time
    pub const fn const_add(self, rhs: Self) -> Self {
        fraction_op!(self, rhs, +).shorten()
    }

    pub const fn const_sub(self, rhs: Self) -> Self {
        fraction_op!(self, rhs, -).shorten()
    }
}

macro_rules! op_assign {
    ($s:ident, $r:ident, $o:tt) => {
        {
            let res = (*$s) $o $r;
            $s.num = res.num;
            $s.den = res.den;
        }
    };
}

impl Add for Fraction {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        fraction_op!(self, rhs, +).shorten()
    }
}

impl AddAssign for Fraction {
    fn add_assign(&mut self, rhs: Self) {
        op_assign!(self, rhs, +)
    }
}

impl Sub for Fraction {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        fraction_op!(self, rhs, -).shorten()
    }
}

impl SubAssign for Fraction {
    fn sub_assign(&mut self, rhs: Self) {
        op_assign!(self, rhs, -)
    }
}

impl Mul for Fraction {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        let num = self.num * rhs.num;
        let den = self.den * rhs.den;
        let mut ret = Self {
            num,
            den
        };
        ret = ret.shorten();
        ret
    }
}

impl MulAssign for Fraction {
    fn mul_assign(&mut self, rhs: Self) {
        op_assign!(self, rhs, *)
    }
}

impl Div for Fraction {
    type Output = Self;
    #[allow(clippy::suspicious_arithmetic_impl)]
    fn div(self, rhs: Self) -> Self::Output {
        (self * rhs.inverse()).shorten()
    }
}

impl DivAssign for Fraction {
    fn div_assign(&mut self, rhs: Self) {
        op_assign!(self, rhs, /)
    }
}

macro_rules! float_from_fract {
    ($t:ty, $f:ty) => {
        impl From<$f> for $t {
            fn from(f: $f) -> Self {
                let num_f = f.num as $t;
                let den_f = f.den as $t;
                num_f / den_f
            }
        }
    };
}


float_from_fract!(f64, Fraction);
float_from_fract!(f64, &Fraction);
float_from_fract!(f32, Fraction);
float_from_fract!(f32, &Fraction);

macro_rules! from_fraction {
    ($t:ty) => {
       impl From<$t> for Fraction {
           fn from(num: $t) -> Self {
               Self {
                   num: num as i64,
                   den: 1
               }
           }
       }
    };
}

impl From<i64> for Fraction {
    fn from(num: i64) -> Self {
        Self {
            num,
            den: 1
        }
    }
}

from_fraction!(isize);
from_fraction!(i128);
from_fraction!(i32);
from_fraction!(i16);
from_fraction!(i8);
from_fraction!(usize);
from_fraction!(u128);
from_fraction!(u64);
from_fraction!(u32);
from_fraction!(u16);
from_fraction!(u8);

#[inline]
const fn const_min(u: u32, v: u32) -> u32 {
    if u < v {
        u
    } else {
        v
    }
}


macro_rules! gcd_macro {
    ($u:ident, $v:ident) => {
        match ($u, $v) {
            (0, d) => {
                return d;
            }
            (d, 0) => {
                return d;
            }
            (mut u_tmp, mut v_tmp) => {
                let i = u_tmp.trailing_zeros();
                let j = v_tmp.trailing_zeros();
                let k = const_min(i, j);

                u_tmp >>= i;
                v_tmp >>= j;

                loop {
                    if u_tmp > v_tmp {
                        (u_tmp, v_tmp) = const_swap!(u_tmp, v_tmp);
                    }

                    v_tmp -= u_tmp;

                    if v_tmp == 0 {
                        return u_tmp << k;
                    }

                    v_tmp >>= v_tmp.trailing_zeros();
                }
            }
        }
    };
}

#[inline]
const fn gcd_i64(u: i64, v: i64) -> i64 {
    gcd_macro!(u, v)
}

#[inline]
const fn gcd_i128(a: i128, b: i128) -> i128 {
    gcd_macro!(a, b)
}

const fn lcm_i64(u: i64, v: i64) -> i64 {
    let u_b = u as i128;
    let v_b = v as i128;
    let mult = (u_b * v_b).abs();
    let cd = gcd_i128(u_b, v_b);
    let ret = mult / cd;
    ret as i64
}

#[cfg(test)]
mod tests {
    use core::{str::FromStr, num::NonZeroI64};

    extern crate alloc;

    use super::Fraction;

    #[test]
    fn sub() {
        let a = Fraction::new(3, 4.try_into().unwrap());
        let b = Fraction::new(1, 4.try_into().unwrap());
        let r = a - b;
        let e = Fraction::new(1, 2.try_into().unwrap());
        assert_eq!(r, e);
    }

    #[test]
    fn add() {
        let a = Fraction::new(1, 2.try_into().unwrap());
        let r = a + a;
        let e = Fraction::from(1);
        assert_eq!(r, e);
    }

    #[test]
    fn mult() {
        let a = Fraction::new(1, 2.try_into().unwrap());
        let b = Fraction::from(2);
        let r = a * b;
        let e = Fraction::from(1);
        assert_eq!(r, e);
    }

    #[test]
    fn parse() {
        let r = Fraction::from_str("1/2").unwrap();
        let e = Fraction::new(1, unsafe { NonZeroI64::new_unchecked(2) });
        assert_eq!(r, e);
        assert_eq!(alloc::format!("{}", r), "1/2");
    }

    #[test]
    #[should_panic]
    fn parse_invalid() {
        let _ = Fraction::from_str("1/0").unwrap();
    }

    #[test]
    fn shorten_zero() {
        extern crate std;

        let zero = Fraction::ZERO;
        zero.shorten();
    }
}
