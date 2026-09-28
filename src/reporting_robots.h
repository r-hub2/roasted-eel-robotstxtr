// Copyright 1999 Google LLC
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     https://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// -----------------------------------------------------------------------------
// File: reporting_robots.h
// -----------------------------------------------------------------------------
//
// This file is a de-Abseiled replica derived from Google's open-source
// `robotstxt` project (https://github.com/google/robotstxt) at the pinned
// commit recorded in PROVENANCE.md. The upstream reporting sources ship without
// a per-file header and are covered by the repository-level Apache-2.0 LICENSE;
// that license and Google's copyright are reproduced here for the vendored copy.
// The only local changes are behavior-neutral Abseil -> C++17 standard-library
// substitutions, catalogued in PROVENANCE.md. Not affiliated with or endorsed
// by Google.

#ifndef THIRD_PARTY_ROBOTSTXT_REPORTING_ROBOTS_H_
#define THIRD_PARTY_ROBOTSTXT_REPORTING_ROBOTS_H_

#include <map>
#include <string_view>
#include <vector>

#include "robots.h"

namespace googlebot {
struct RobotsParsedLine {
  enum RobotsTagName {
    // Identifier for skipped lines. A line may be skipped because it's
    // unparseable, or because it contains no recognizable key. Note that
    // comment lines are also skipped, they're no-op for parsing. For example:
    //   random characters
    //   unicorn: <value>
    //   # comment line
    // Same thing for empty lines.
    kUnknown = 0,
    kUserAgent = 1,
    kAllow = 2,
    kDisallow = 3,
    kSitemap = 4,
    // Identifier for parseable lines whose key is recognized, but unused.
    // See kUnsupportedTags in reporting_robots.cc for a list of recognized,
    // but unused keys. For example:
    //   noindex: <value>
    //   noarchive: <value>
    kUnused = 5,
  };

  int line_num = 0;
  RobotsTagName tag_name = kUnknown;
  bool is_typo = false;
  RobotsParseHandler::LineMetadata metadata;
};

class RobotsParsingReporter : public googlebot::RobotsParseHandler {
 public:
  void HandleRobotsStart() override;
  void HandleRobotsEnd() override;
  void HandleUserAgent(int line_num, std::string_view line_value) override;
  void HandleAllow(int line_num, std::string_view line_value) override;
  void HandleDisallow(int line_num, std::string_view line_value) override;
  void HandleSitemap(int line_num, std::string_view line_value) override;
  void HandleUnknownAction(int line_num, std::string_view action,
                           std::string_view line_value) override;
  void ReportLineMetadata(int line_num, const LineMetadata& metadata) override;

  int last_line_seen() const { return last_line_seen_; }
  int valid_directives() const { return valid_directives_; }
  int unused_directives() const { return unused_directives_; }
  std::vector<RobotsParsedLine> parse_results() const {
    std::vector<RobotsParsedLine> vec;
    for (const auto& entry : robots_parse_results_) {
      vec.push_back(entry.second);
    }
    return vec;
  }

 private:
  void Digest(int line_num, RobotsParsedLine::RobotsTagName parsed_tag);

  // Indexed and sorted by line number. `std::map` (an ordered red-black tree
  // keyed by the int line number) replaces upstream's `absl::btree_map`; both
  // iterate keys in ascending order, so `parse_results()` yields the exact same
  // observable sorted-by-line sequence.
  std::map<int, RobotsParsedLine> robots_parse_results_;
  int last_line_seen_ = 0;
  int valid_directives_ = 0;
  int unused_directives_ = 0;
};
}  // namespace googlebot
#endif  // THIRD_PARTY_ROBOTSTXT_REPORTING_ROBOTS_H_
