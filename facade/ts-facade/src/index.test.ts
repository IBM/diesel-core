/*
 * Copyright 2018 The Diesel Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import {DieselParserFacade, ParseRequest, PredictRequest} from "./index";

// @ts-ignore
import * as MyBundle from "my-dsl-bundle";
import {expect} from "chai";

function getMyParser(): DieselParserFacade {
  // @ts-ignore
  return MyBundle.MyFacade.createMyParser();
}

describe('parse', () => {
  it("parser should be defined", () => {
    const p = getMyParser();
    expect(p).to.be.not.undefined;
  });
  it("parser should parse", () => {
    const parseRequest: ParseRequest = { text: "a x is a concept." };
    const res = getMyParser().parse(parseRequest);
    expect(res.error).to.be.undefined;
    expect(res.success).to.equal(true);
    expect(res.markers.length).to.equal(0);
    expect(res.styles.length).to.equal(1);
    const s0 = res.styles[0];
    expect(s0.offset).to.equal(9)
    expect(s0.length).to.equal(7)
    expect(s0.name).to.equal("keyword");
  });
  it("parser should predict", () => {
    const predictRequest: PredictRequest = { text: "", offset: 0} ;
    const res = getMyParser().predict(predictRequest);
    expect(res.error).to.be.undefined;
    expect(res.success).to.equal(true);
    expect(res.proposals.length).to.equal(1);
    const p0 = res.proposals[0];
    expect(p0.text).to.equal("a");
  });
});
