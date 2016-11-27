public class RubyMath {
    /** Create the Math module and add it to the Ruby runtime.
     * 
     */

    @JRubyMethod(name = "sqrt", required = 1, module = true, visibility = Visibility.PRIVATE, compat = CompatVersion.RUBY1_8)
    public static RubyFloat sqrt(IRubyObject recv, IRubyObject x) {
        double value = ((RubyFloat)RubyKernel.new_float(recv,x)).getDoubleValue();
        double result;

        if (value < 0) {
            result = Double.NaN;
        } else{
            result = Math.sqrt(value);
        }
        
        domainCheck(recv, result, "sqrt");
        return RubyFloat.newFloat(recv.getRuntime(), result);
    }

    @JRubyMethod(name = "sqrt", required = 1, module = true, visibility = Visibility.PRIVATE, compat = CompatVersion.RUBY1_9)
    public static RubyFloat sqrt19(IRubyObject recv, IRubyObject x) {
        double value = needFloat(x).getDoubleValue();
        double result;

        if (value < 0) {
            result = Double.NaN;
        } else{
            result = Math.sqrt(value);
        }
        
        domainCheck19(recv, result, "sqrt");
        return RubyFloat.newFloat(recv.getRuntime(), result);
    }
    
    @JRubyMethod(name = "cbrt", required = 1, module = true, visibility = Visibility.PRIVATE, compat = CompatVersion.RUBY1_9)
    public static RubyFloat cbrt(IRubyObject recv, IRubyObject x) {
        double value = ((RubyFloat)RubyKernel.new_float(recv,x)).getDoubleValue();
        double result;

        if (value < 0) {
            result = -Math.pow(-value, 1/3.0);
        } else{
            result = Math.pow(value, 1/3.0);
        }

        domainCheck(recv, result, "cbrt");
        return RubyFloat.newFloat(recv.getRuntime(), result);
    }

    @JRubyMethod(name = "hypot", required = 2, module = true, visibility = Visibility.PRIVATE, compat = CompatVersion.RUBY1_8)
    public static RubyFloat hypot(IRubyObject recv, IRubyObject x, IRubyObject y) {
        double valuea = ((RubyFloat)RubyKernel.new_float(recv,x)).getDoubleValue(); 
        double valueb = ((RubyFloat)RubyKernel.new_float(recv,y)).getDoubleValue();
        double result;
        
        if (Math.abs(valuea) > Math.abs(valueb)) {
            result = valueb / valuea;
            result = Math.abs(valuea) * Math.sqrt(1 + result * result);
        } else if (valueb != 0) {
            result = valuea / valueb;
            result = Math.abs(valueb) * Math.sqrt(1 + result * result);
        } else {
            result = 0;
        }
        return RubyFloat.newFloat(recv.getRuntime(),result);
    }    

    @JRubyMethod(name = "hypot", required = 2, module = true, visibility = Visibility.PRIVATE, compat = CompatVersion.RUBY1_9)
    public static RubyFloat hypot19(IRubyObject recv, IRubyObject x, IRubyObject y) {
        double valuea = needFloat(x).getDoubleValue(); 
        double valueb = needFloat(y).getDoubleValue();
        double result;
        
        if (Math.abs(valuea) > Math.abs(valueb)) {
            result = valueb / valuea;
            result = Math.abs(valuea) * Math.sqrt(1 + result * result);
        } else if (valueb != 0) {
            result = valuea / valueb;
            result = Math.abs(valueb) * Math.sqrt(1 + result * result);
        } else {
            result = 0;
        }
        return RubyFloat.newFloat(recv.getRuntime(),result);
    }    
    
    
    /*
     * x = mantissa * 2 ** exponent
     *
     * Where mantissa is in the range of [.5, 1)
     *
     */
    @JRubyMethod(name = "frexp", required = 1, module = true, visibility = Visibility.PRIVATE, compat = CompatVersion.RUBY1_8)
    public static RubyArray frexp(IRubyObject recv, IRubyObject other) {
        double mantissa = ((RubyFloat)RubyKernel.new_float(recv,other)).getDoubleValue();
        short sign = 1;
        long exponent = 0;

        if (!Double.isInfinite(mantissa) && mantissa != 0.0) {
            // Make mantissa same sign so we only have one code path.
            if (mantissa < 0) {
                mantissa = -mantissa;
                sign = -1;
            }

            // Increase value to hit lower range.
            for (; mantissa < 0.5; mantissa *= 2.0, exponent -=1) { }

            // Decrease value to hit upper range.  
            for (; mantissa >= 1.0; mantissa *= 0.5, exponent +=1) { }
        }
	 
        return RubyArray.newArray(recv.getRuntime(), 
                                 RubyFloat.newFloat(recv.getRuntime(), sign * mantissa),
                                 RubyNumeric.int2fix(recv.getRuntime(), exponent));
    }

    @JRubyMethod(name = "frexp", required = 1, module = true, visibility = Visibility.PRIVATE, compat = CompatVersion.RUBY1_9)
    public static RubyArray frexp19(IRubyObject recv, IRubyObject other) {
        double mantissa = needFloat(other).getDoubleValue();
        short sign = 1;
        long exponent = 0;

        if (!Double.isInfinite(mantissa) && mantissa != 0.0) {
            // Make mantissa same sign so we only have one code path.
            if (mantissa < 0) {
                mantissa = -mantissa;
                sign = -1;
            }

            // Increase value to hit lower range.
            for (; mantissa < 0.5; mantissa *= 2.0, exponent -=1) { }

            // Decrease value to hit upper range.  
            for (; mantissa >= 1.0; mantissa *= 0.5, exponent +=1) { }
        }
	 
        return RubyArray.newArray(recv.getRuntime(), 
                                 RubyFloat.newFloat(recv.getRuntime(), sign * mantissa),
                                 RubyNumeric.int2fix(recv.getRuntime(), exponent));
    }

    /*
     * r = x * 2 ** y
     */
    @JRubyMethod(name = "ldexp", required = 2, module = true, visibility = Visibility.PRIVATE, compat = CompatVersion.RUBY1_8)
    public static RubyFloat ldexp(IRubyObject recv, IRubyObject mantissa, IRubyObject exponent) {
        double mantissaValue = ((RubyFloat)RubyKernel.new_float(recv, mantissa)).getDoubleValue();
        return RubyFloat.newFloat(recv.getRuntime(),mantissaValue * Math.pow(2.0, RubyNumeric.num2int(exponent)));
    }

    @JRubyMethod(name = "ldexp", required = 2, module = true, visibility = Visibility.PRIVATE, compat = CompatVersion.RUBY1_9)
    public static RubyFloat ldexp19(IRubyObject recv, IRubyObject mantissa, IRubyObject exponent) {
        double mantissaValue = needFloat(mantissa).getDoubleValue();
        return RubyFloat.newFloat(recv.getRuntime(),mantissaValue * Math.pow(2.0, RubyNumeric.num2int(exponent)));
    }

    private static final double ERFC_COEF[] = {
         -.490461212346918080399845440334e-1,
         -.142261205103713642378247418996e0,
         .100355821875997955757546767129e-1,
         -.576876469976748476508270255092e-3,
         .274199312521960610344221607915e-4,
         -.110431755073445076041353812959e-5,
         .384887554203450369499613114982e-7,
         -.118085825338754669696317518016e-8,
         .323342158260509096464029309534e-10,
         -.799101594700454875816073747086e-12,
         .179907251139614556119672454866e-13,
         -.371863548781869263823168282095e-15,
         .710359900371425297116899083947e-17,
         -.126124551191552258324954248533e-18
    };
    
    @JRubyMethod(name = "erf", required = 1, module = true, visibility = Visibility.PRIVATE, compat = CompatVersion.RUBY1_8)
    public static RubyFloat erf(IRubyObject recv, IRubyObject x) {
        double value = ((RubyFloat)RubyKernel.new_float(recv,x)).getDoubleValue();

        double  result;
        double  y = Math.abs(value);

        if (y <= 1.49012e-08) {
            result = 2 * value / 1.77245385090551602729816748334;
        } else if (y <= 1) {
            result = value * (1 + chebylevSerie(2 * value * value - 1, ERFC_COEF));
        } else if (y < 6.013687357) {
            result = sign(1 - erfc(recv, RubyFloat.newFloat(recv.getRuntime(),y)).getDoubleValue(), value);
        } else {
            result = sign(1, value);
        }
        return RubyFloat.newFloat(recv.getRuntime(),result);
    }

    @JRubyMethod(name = "erf", required = 1, module = true, visibility = Visibility.PRIVATE, compat = CompatVersion.RUBY1_9)
    public static RubyFloat erf19(IRubyObject recv, IRubyObject x) {
        double value = needFloat(x).getDoubleValue();

        double  result;
        double  y = Math.abs(value);

        if (y <= 1.49012e-08) {
            result = 2 * value / 1.77245385090551602729816748334;
        } else if (y <= 1) {
            result = value * (1 + chebylevSerie(2 * value * value - 1, ERFC_COEF));
        } else if (y < 6.013687357) {
            result = sign(1 - erfc(recv, RubyFloat.newFloat(recv.getRuntime(),y)).getDoubleValue(), value);
        } else {
            result = sign(1, value);
        }
        return RubyFloat.newFloat(recv.getRuntime(),result);
    }

    private static final double ERFC2_COEF[] = {
         -.69601346602309501127391508262e-1,
         -.411013393626208934898221208467e-1,
         .391449586668962688156114370524e-2,
         -.490639565054897916128093545077e-3,
         .715747900137703638076089414183e-4,
         -.115307163413123283380823284791e-4,
         .199467059020199763505231486771e-5,
         -.364266647159922287393611843071e-6,
         .694437261000501258993127721463e-7,
         -.137122090210436601953460514121e-7,
         .278838966100713713196386034809e-8,
         -.581416472433116155186479105032e-9,
         .123892049175275318118016881795e-9,
         -.269063914530674343239042493789e-10,
         .594261435084791098244470968384e-11,
         -.133238673575811957928775442057e-11,
         .30280468061771320171736972433e-12,
         -.696664881494103258879586758895e-13,
         .162085454105392296981289322763e-13,
         -.380993446525049199987691305773e-14,
         .904048781597883114936897101298e-15,
         -.2164006195089607347809812047e-15,
         .522210223399585498460798024417e-16,
         -.126972960236455533637241552778e-16,
         .310914550427619758383622741295e-17,
         -.766376292032038552400956671481e-18,
         .190081925136274520253692973329e-18
    };

    private static final double ERFCC_COEF[] = {
         .715179310202924774503697709496e-1,
         -.265324343376067157558893386681e-1,
         .171115397792085588332699194606e-2,
         -.163751663458517884163746404749e-3,
         .198712935005520364995974806758e-4,
         -.284371241276655508750175183152e-5,
         .460616130896313036969379968464e-6,
         -.822775302587920842057766536366e-7,
         .159214187277090112989358340826e-7,
         -.329507136225284321486631665072e-8,
         .72234397604005554658126115389e-9,
         -.166485581339872959344695966886e-9,
         .401039258823766482077671768814e-10,
         -.100481621442573113272170176283e-10,
         .260827591330033380859341009439e-11,
         -.699111056040402486557697812476e-12,
         .192949233326170708624205749803e-12,
         -.547013118875433106490125085271e-13,
         .158966330976269744839084032762e-13,
         -.47268939801975548392036958429e-14,
         .14358733767849847867287399784e-14,
         -.444951056181735839417250062829e-15,
         .140481088476823343737305537466e-15,
         -.451381838776421089625963281623e-16,
         .147452154104513307787018713262e-16,
         -.489262140694577615436841552532e-17,
         .164761214141064673895301522827e-17,
         -.562681717632940809299928521323e-18,
         .194744338223207851429197867821e-18
    };
        
    @JRubyMethod(name = "erfc", required = 1, module = true, visibility = Visibility.PRIVATE, compat = CompatVersion.RUBY1_8)
    public static RubyFloat erfc(IRubyObject recv, IRubyObject x) {
        double value = ((RubyFloat)RubyKernel.new_float(recv,x)).getDoubleValue();
        double  result;
        double  y = Math.abs(value);

        if (value <= -6.013687357) {
            result = 2;
        } else if (y < 1.49012e-08) {
            result = 1 - 2 * value / 1.77245385090551602729816748334;
        } else {
            double ysq = y*y;
            if (y < 1) {
                result = 1 - value * (1 + chebylevSerie(2 * ysq - 1, ERFC_COEF));
            } else if (y <= 4.0) {
                result = Math.exp(-ysq)/y*(0.5+chebylevSerie((8.0 / ysq - 5.0) / 3.0, ERFC2_COEF));
                if (value < 0) result = 2.0 - result;
                if (value < 0) result = 2.0 - result;
                if (value < 0) result = 2.0 - result;
            } else {
                result = Math.exp(-ysq) / y * (0.5 + chebylevSerie(8.0 / ysq - 1, ERFCC_COEF));
                if (value < 0) result = 2.0 - result;
            }
        }
        return RubyFloat.newFloat(recv.getRuntime(),result);        
    }

    @JRubyMethod(name = "erfc", required = 1, module = true, visibility = Visibility.PRIVATE, compat = CompatVersion.RUBY1_9)
    public static RubyFloat erfc19(IRubyObject recv, IRubyObject x) {
        double value = needFloat(x).getDoubleValue();
        double  result;
        double  y = Math.abs(value);

        if (value <= -6.013687357) {
            result = 2;
        } else if (y < 1.49012e-08) {
            result = 1 - 2 * value / 1.77245385090551602729816748334;
        } else {
            double ysq = y*y;
            if (y < 1) {
                result = 1 - value * (1 + chebylevSerie(2 * ysq - 1, ERFC_COEF));
            } else if (y <= 4.0) {
                result = Math.exp(-ysq)/y*(0.5+chebylevSerie((8.0 / ysq - 5.0) / 3.0, ERFC2_COEF));
                if (value < 0) result = 2.0 - result;
                if (value < 0) result = 2.0 - result;
                if (value < 0) result = 2.0 - result;
            } else {
                result = Math.exp(-ysq) / y * (0.5 + chebylevSerie(8.0 / ysq - 1, ERFCC_COEF));
                if (value < 0) result = 2.0 - result;
            }
        }
        return RubyFloat.newFloat(recv.getRuntime(),result);        
    }

    private static final double FACTORIAL[] = {
        /*  0! */ 1.0,
        /*  1! */ 1.0,
        /*  2! */ 2.0,
        /*  3! */ 6.0,
        /*  4! */ 24.0,
        /*  5! */ 120.0,
        /*  6! */ 720.0,
        /*  7! */ 5040.0,
        /*  8! */ 40320.0,
        /*  9! */ 362880.0,
        /* 10! */ 3628800.0,
        /* 11! */ 39916800.0,
        /* 12! */ 479001600.0,
        /* 13! */ 6227020800.0,
        /* 14! */ 87178291200.0,
        /* 15! */ 1307674368000.0,
        /* 16! */ 20922789888000.0,
        /* 17! */ 355687428096000.0,
        /* 18! */ 6402373705728000.0,
        /* 19! */ 121645100408832000.0,
        /* 20! */ 2432902008176640000.0,
        /* 21! */ 51090942171709440000.0,
        /* 22! */ 1124000727777607680000.0
    };

    private static final double NEMES_GAMMA_COEFF[] = {
        1.00000000000000000000000000000000000,
        0                                    ,
        0.08333333333333333333333333333333333,
        0                                    ,
        0.00069444444444444444444444444444444,
        0                                    ,
        0.00065861992945326278659611992945326,
        0                                    ,
       -0.00053287817827748383303938859494415,
        0                                    ,
        0.00079278588700608376534302460228386,
        0                                    ,
       -0.00184758189322033028400606295961969,
        0                                    ,
        0.00625067824784941846328836824623616,
        0                                    ,
       -0.02901710246301150993444701506844402,
        0                                    ,
        0.17718457242491308890302832366796470,
        0                                    ,
       -1.37747681703993534399676348903067470
    };


    /**
     * Based on Gerg&#337; Nemes's Gamma Function approximation formula, we compute
     * approximate value of Gamma function of x.
     * @param recv Math module
     * @param x a real number
     * @return &Gamma;(x) for real number x
     * @see <a href="http://www.ebyte.it/library/downloads/2008_MTH_Nemes_GammaApproximationUpdate.pdf">
     * New asymptotic expansion for the &Gamma;(x) function</a>
     */

    @JRubyMethod(name = "gamma", required = 1, module = true, visibility = Visibility.PRIVATE, compat = CompatVersion.RUBY1_9)
    public static RubyFloat gamma(IRubyObject recv, IRubyObject x) {
        double value = ((RubyFloat) RubyKernel.new_float(recv, x)).getDoubleValue();
        double result = nemes_gamma(value);
        /* note nemes_gamma can return Double.POSITIVE_INFINITY or Double.NEGATIVE_INFINITY
         * when value is an integer less than 1.
         * We treat 0 as a special case to avoid Domain error.
         */
        if (Double.isInfinite(result)) {
            if (value < 0) {
                result = Double.NaN;
            } else {
                result = Double.POSITIVE_INFINITY;
            }
        }

        if (Double.isNaN(value)) {
            return RubyFloat.newFloat(recv.getRuntime(), Double.NaN);
        }

        domainCheck19(recv, result, "gamma");
        return RubyFloat.newFloat(recv.getRuntime(), result);

    }

    /**
     * Based on Gerg&#337; Nemes's Gamma Function approximation formula, we compute
     * Log Gamma function for real number x.
     * @param recv Math module
     * @param x a real number
     * @return 2-element array [ln(&Gamma;(x)), sgn] for real number x,
     *  where sgn is the sign of &Gamma;(x) when exponentiated
     * @see #gamma(org.jruby.runtime.builtin.IRubyObject, org.jruby.runtime.builtin.IRubyObject)
     */

    @JRubyMethod(name = "lgamma", required = 1, module = true, visibility = Visibility.PRIVATE, compat = CompatVersion.RUBY1_9)
    public static RubyArray lgamma(IRubyObject recv, IRubyObject x) {
        Ruby runtime      = recv.getRuntime();
        double value      = RubyKernel.new_float(recv, x).getDoubleValue();
        // JRUBY-4653: Could this error checking done more elegantly?
        if (value < 0 && Double.isInfinite(value)) {
            throw recv.getRuntime().newMathDomainError("lgamma");
        }

        NemesLogGamma l   = new NemesLogGamma(value);
        IRubyObject[] ary = new IRubyObject[2];
        ary[0] = RubyFloat.newFloat(runtime, l.value);
        ary[1] = RubyInteger.int2fix(runtime, (int) l.sign);

        return RubyArray.newArray(recv.getRuntime(), ary);
    }

    private static double nemes_gamma(double x) {
        double int_part = (int) x;

        if ((x - int_part) == 0.0 && 0 < int_part && int_part <= FACTORIAL.length) {
            return FACTORIAL[(int) int_part - 1];
        }
        NemesLogGamma l = new NemesLogGamma(x);
        return l.sign * Math.exp(l.value);
    }

    /**
     * Inner class to help with &Gamma; functions
     */
    private static class NemesLogGamma {
        double value;
        double sign = 1;

        private NemesLogGamma(double x) {
            if (Double.isInfinite(x)) {
                value = Double.POSITIVE_INFINITY;
                return;
            }

            if (Double.isNaN(x)) {
                value = Double.NaN;
                return;
            }

            double int_part = (int) x;
            sign = (int_part % 2 == 0 && (x - int_part) != 0.0 && (x < 0)) ? -1 : 1;
            if ((x - int_part) == 0.0 && 0 < int_part && int_part <= FACTORIAL.length) {
                value = Math.log(FACTORIAL[(int) int_part - 1]);
            }
            else if (x < 10) {
                double rising_factorial = 1;
                for (int i = 0; i < (int) Math.abs(x) - int_part + 10; i++) {
                    rising_factorial *= (x + i);
                }
                NemesLogGamma l = new NemesLogGamma(x + (int) Math.abs(x) - int_part + 10);
                value = l.value - Math.log(Math.abs(rising_factorial));
            } else {
                double temp = 0.0;
                for (int i = 0; i < NEMES_GAMMA_COEFF.length; i++) {
                    temp += NEMES_GAMMA_COEFF[i] * 1.0 / Math.pow(x, i);
                }

                value = x * (Math.log(x) - 1 + Math.log(temp)) +
                        (Math.log(2) + Math.log(Math.PI) - Math.log(x)) / 2.0;
            }
        }
    }
}
