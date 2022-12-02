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

export interface ParseRequest {
  readonly text: string;
  readonly axiom?: string;
}

export interface PredictRequest extends ParseRequest {
  readonly offset: number;
}

export interface HasRange {
  readonly offset: number;
  readonly length: number;
}

export interface DieselMarker extends HasRange {
  readonly severity: string;
  getMessage(locale: string): string;
}

export interface DieselStyle extends HasRange {
  readonly name: string;
}

export interface HasSuccessAndError {
  readonly success: boolean;
  readonly error?: string;
}

export interface DieselParseResult extends HasSuccessAndError {
  readonly markers: ReadonlyArray<DieselMarker>;
  readonly styles: ReadonlyArray<DieselStyle>;
}

export interface DieselCompletionProposal {
  readonly text: string;
  readonly replace?: HasRange;
}

export interface DieselPredictResult extends HasSuccessAndError {
  readonly proposals: ReadonlyArray<DieselCompletionProposal>;
}

export interface DieselParserFacade {
  parse(request: ParseRequest): DieselParseResult;
  predict(request: PredictRequest): DieselPredictResult;
}
