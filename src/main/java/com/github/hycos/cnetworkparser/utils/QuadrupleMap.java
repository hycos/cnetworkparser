/*
 * cnetworkparser - generate constraint network from different formats
 * Copyright (C) 2017 Julian Thome <julian.thome.de@gmail.com>
 *
 * cnetworkparser is licensed under the EUPL, Version 1.1 or – as soon
 * they will be approved by the European Commission - subsequent versions of the
 * EUPL (the "Licence"); You may not use this work except in compliance with the
 * Licence. You may obtain a copy of the Licence at:
 *
 * https://joinup.ec.europa.eu/sites/default/files/eupl1.1.-licence-en_0.pdf
 *
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the Licence is distributed on an "AS IS" basis, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the Licence for the
 * specific language governing permissions and limitations under the Licence.
 */

package com.github.hycos.cnetworkparser.utils;


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
