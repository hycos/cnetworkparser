package org.snt.cnetworkparser.utils;


public class Quadruple<FIRST, SECOND, THIRD, FOURTH> {

    private FIRST first;
    private SECOND second;
    private THIRD third;
    private FOURTH fourth;

    public FIRST getFirst() {
        return first;
    }

    public SECOND getSecond() {
        return second;
    }

    public THIRD getThird() {
        return third;
    }

    public FOURTH getFourth() {
        return fourth;
    }

    public Quadruple(FIRST first, SECOND second, THIRD third, FOURTH fourth){
        this.first = first;
        this.second = second;
        this.third = third;
        this.fourth = fourth;
    }

    @Override
    public int hashCode() {
        return first.hashCode() ;
    }

    @Override
    public boolean equals(Object o) {
        if(!(o instanceof Quadruple)){
            return false;
        }

        Quadruple l = (Quadruple)o;

        return l.first.equals(this.first);
    }

}
