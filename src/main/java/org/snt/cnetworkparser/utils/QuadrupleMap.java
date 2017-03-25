package org.snt.cnetworkparser.utils;


import java.util.HashMap;
import java.util.Map;

public class QuadrupleMap<FIRST,SECOND,THIRD,FOURTH> {

    private Map<FIRST,Quadruple<FIRST,SECOND,THIRD,FOURTH>> fmap;
    private Map<SECOND,FIRST> smap;


    public QuadrupleMap() {
        this.fmap = new HashMap<>();
        this.smap = new HashMap<>();
    }

    public void put(FIRST first, SECOND second ,THIRD third, FOURTH fourth) {
        this.fmap.put(first, new Quadruple(first, second, third, fourth));
        this.smap.put(second,first);
    }

    public Quadruple get(FIRST first) {
        return this.fmap.get(first);
    }

    public SECOND getSecondByFirst(FIRST first) {
        if(this.fmap.containsKey(first)){
            return this.fmap.get(first).getSecond();
        }
        return null;
    }

    public THIRD getThirdByFirst(FIRST first) {
        if(this.fmap.containsKey(first)){
            return this.fmap.get(first).getThird();
        }
        return null;
    }

    public FOURTH getFourthByFirst(FIRST first) {
        if(this.fmap.containsKey(first)){
            return this.fmap.get(first).getFourth();
        }
        return null;
    }



    public THIRD getThirdBySecond(SECOND second) {
        if(this.smap.containsKey(second)){
            FIRST first = this.smap.get(second);
            return this.fmap.get(first).getThird();
        }
        return null;
    }

    public FOURTH getFourthBySecond(SECOND second) {
        if(this.smap.containsKey(second)){
            FIRST first = this.smap.get(second);
            return this.fmap.get(first).getFourth();
        }
        return null;
    }


    public boolean containsKey(FIRST first) {
        return this.fmap.containsKey(first);
    }
}
