use core::{ops::{Index, IndexMut}, fmt::Debug};

use crate::fraction::Fraction;


macro_rules! const_for {
    (for $i:ident in $l:expr,$u:expr => $b:block) => {
        let mut $i = $l;
        while $i < $u {
            $b
            $i += 1;
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct StaticMatrix<const HEIGHT: usize, const WIDTH: usize>(
    [[Fraction; WIDTH]; HEIGHT]
);

impl<const HEIGHT: usize, const WIDTH: usize> Debug for StaticMatrix<HEIGHT, WIDTH> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        for i in 0..HEIGHT {
            f.debug_list().entries(self[i].iter()).finish()?;
        }
        Ok(())
    }
}

impl<const LENGTH: usize> StaticMatrix<LENGTH, LENGTH> {
    pub const IDENTITY: Self = {
        let mut ret = Self::ZERO;
        const_for!(for i in 0,LENGTH => {
            ret.0[i][i] = Fraction::new_int(1);
        });
        ret
    };
}


impl<const HEIGHT: usize, const WIDTH: usize> StaticMatrix<HEIGHT, WIDTH> {
    pub const ZERO: Self = Self([[Fraction::ZERO; WIDTH]; HEIGHT]);

    pub const fn new_int(values: [[i64; WIDTH]; HEIGHT]) -> Self {
        let mut ret = Self::ZERO;
        const_for!(for i in 0,HEIGHT => {
            const_for!(for j in 0,WIDTH => {
                let fract = Fraction::new_int(values[i][j]);
                ret.0[i][j] = fract;
            });
        });

        ret
    }

    pub const fn to_int_num(&self) -> Option<[[i64; WIDTH]; HEIGHT]> {
        let mut ret = [[0; WIDTH]; HEIGHT];
        const_for!(for i in 0,HEIGHT => {
            const_for!(for j in 0,WIDTH => {
                let fraction_res = self.0[i][j].to_int();
                match fraction_res {
                    Some(d) => {
                        ret[i][j] = d;
                    },
                    None => return None,
                }
            });
        });
        Some(ret)
    }

    pub fn to_float_num(&self) -> [[f64; WIDTH]; HEIGHT] {
        let mut ret = [[0.0f64; WIDTH]; HEIGHT];
        const_for!(for i in 0,HEIGHT => {
            const_for!(for j in 0,WIDTH => {
                let fraction_res = self.0[i][j].to_float();
                ret[i][j] = fraction_res;
            });
        });
        ret
    }

    pub const fn to_frac_num(&self) -> [[Fraction; WIDTH]; HEIGHT] {
        self.0
    }

    pub const fn column(&self, index: usize) -> [Fraction; HEIGHT] {
        let mut ret = [Fraction::ZERO; HEIGHT];

        const_for!(for i in 0,HEIGHT => {
            ret[i] = self.0[i][index]; 
        });

        ret
    }

    pub const fn mult<const R_WIDTH: usize>(&self, rhs: &StaticMatrix<WIDTH, R_WIDTH>) -> StaticMatrix<HEIGHT, R_WIDTH> {
        let mut ret = StaticMatrix::<HEIGHT, R_WIDTH>::ZERO;
        const_for!(for i in 0,HEIGHT => {
            let lane = self.0[i];
            const_for!(for j in 0,R_WIDTH => {
                let column = rhs.column(j);
                let mut cell_val = Fraction::ZERO;
                const_for!(for z in 0,WIDTH => {
                    cell_val = cell_val.const_add(lane[z].const_mul(column[z]));
                });
                ret.0[i][j] = cell_val;
            });
        });
        ret
    }

    pub const fn add(&self, rhs: &StaticMatrix<HEIGHT, WIDTH>) -> StaticMatrix<HEIGHT, WIDTH> {
        let mut ret = Self::ZERO;
        const_for!(for i in 0,HEIGHT => {
            const_for!(for j in 0,WIDTH => {
                let self_side = self.0[i][j];
                let right_side = rhs.0[i][j];
                ret.0[i][j] = self_side.const_add(right_side);
            });
        });
        ret
    }
}



impl<const WIDTH: usize, const HEIGHT: usize, T: Into<usize>> Index<T> for StaticMatrix<HEIGHT, WIDTH> {
    type Output = [Fraction; WIDTH];
    fn index(&self, index: T) -> &Self::Output {
        &self.0[index.into()]
    }
}

impl<const WIDTH: usize, const HEIGHT: usize, T: Into<usize>> IndexMut<T> for StaticMatrix<HEIGHT, WIDTH> {
    fn index_mut(&mut self, index: T) -> &mut Self::Output {
        &mut self.0[index.into()]
    }
}

#[cfg(test)]
mod tests {

    use crate::fraction::Fraction;

    use super::StaticMatrix;


    #[test]
    fn column() {
        extern crate std;
        let identity = StaticMatrix::<4, 4>::identity().unwrap();
        let column = identity.column(0);
        let expected = [Fraction::ONE, Fraction::ZERO, Fraction::ZERO, Fraction::ZERO];
        assert_eq!(column, expected);
    }

    #[test]
    fn mult_ident() {
        let identity = StaticMatrix::<4, 4>::identity().unwrap();
        let res = identity.mult(&identity);
        assert_eq!(res, identity);
    }

    #[test]
    #[allow(non_upper_case_globals)]
    fn new_int_mult() {
        const a_content: [[i64; 3]; 2] = [
            [
                1, 2, 3
            ],
            [
                2, 3, 4
            ]
        ];

        const b_content: [[i64; 2]; 3] = [
            [
                1, 2
            ],
            [
                2, 3
            ],
            [
                3, 4
            ]
        ];
        
        const a: StaticMatrix<2, 3> = StaticMatrix::new_int(a_content);
        const b: StaticMatrix<3, 2> = StaticMatrix::new_int(b_content);
        const c: StaticMatrix<3, 3> = b.mult(&a);
        let expected_c_content = [
            [
                5, 8, 11
            ],
            [
                8, 13, 18
            ],
            [
                11, 18, 25
            ]
        ];
        let real_c_content = c.to_int_num().unwrap();
        assert_eq!(expected_c_content, real_c_content);
    }
}
