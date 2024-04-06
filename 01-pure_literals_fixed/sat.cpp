#include <algorithm>
#include <errno.h>
#include <fstream>
#include <iostream>
#include <set>
#include <sstream>
#include <string>
#include <string.h>
#include <vector>

#define MAXVARS 1500

using std::vector;
using std::string;
using std::set;

class CNF {
    int nvars;
    bool empty = false;
    vector<vector<int> > clauses;

public:
    static CNF* parse(const string& name) {
        std::ifstream f(name);
        if (f.fail()) {
            std::cout << "Cannot open file: " << strerror(errno) << "\n";
            return nullptr;
        }
        string s;
        int line = 0;
        int nclauses;
        string tmp;

        CNF* result = new CNF();

        while (1) {
            line++;
            std::getline(f, s);
            if (s[0] == 'c')
                continue;
            if (s[0] == 'p')
                break;
            std::cout << "Invalid DIMACS, unknown letter at line " << line << "\n";
            return nullptr;
        }
        std::istringstream iss(s);
        iss >> tmp >> tmp >> result->nvars >> nclauses;
        if (result->nvars > MAXVARS) {
            std::cout << "too many vars\n";
            exit(1);
        }
        for (int i = 0; i < nclauses; i++) {
            vector<int> clause;
            int literal;
            std::getline(f, s);
            std::istringstream iss(s);
            set<int> used_literals; // for sanity checks
            bool clause_always_true = false;

            iss >> literal;
            while (literal) {
                if (used_literals.find(literal) != used_literals.end()) {
                    iss >> literal;
                    continue;
                }
                if (used_literals.find(-literal) != used_literals.end())
                    clause_always_true = true;
                used_literals.insert(literal);
                clause.push_back(literal);
                iss >> literal;
            } while (literal);
            if (!clause_always_true)
                result->clauses.push_back(clause);
        }
        return result;
    }

    set<int> find_unit_clauses() const {
        set<int> ret;
        for (const auto& a : clauses) {
            if (a.size() == 1)
                ret.insert(a[0]);
        }
        return ret;
    }

    set<int> find_pure_literals() const {
        set<int> ret;
        set<int> bad_vars;

        for (const auto& c : clauses) {
            for (int l : c) {
                if (bad_vars.find(abs(l)) != bad_vars.end()) continue;
                if (ret.find(l) != ret.end()) continue;
                if (ret.find(-l) != ret.end()) {
                    bad_vars.insert(abs(l));
                    ret.erase(-l);
                } else {
                    ret.insert(l);
                }
            }
        }
        return ret;
    }

    bool has_empty_clause() const { return empty; }
    bool is_cnf_empty() const { return clauses.size() == 0; }

    void propagate_var_single_value(int v) {
        clauses.erase(std::remove_if(clauses.begin(), clauses.end(), [v](const vector<int>& c) { return std::find(c.begin(), c.end(), v) != c.end(); }), clauses.end());
        for (auto& c : clauses) {
            c.erase(std::remove_if(c.begin(), c.end(), [v](int l){ return l == -v; }), c.end());
            if (c.size() == 0) {
                empty = true;
                return;
            }
        }
    }

    void propagate_var_values(const set<int>& vs) {
        for (int v : vs) {
            propagate_var_single_value(v);
            if (empty)
                break;
        }
    }

    int select_branching_literal() {
        return clauses[0][0];
    }

    bool solve_helper(vector<int>* acc) {
        if (has_empty_clause()) return false;

        // unit clauses
        while (true) {
            set<int> units = find_unit_clauses();
            if (units.size() == 0) break;
            if (units.size() > 1) {
                for (int v : units) {
                    if (units.find(-v) != units.end()) return false;
                }
            }
            for (int v : units) acc->push_back(v);
            propagate_var_values(units);
            if (has_empty_clause()) return false;
        }

        // pure literals
        while (true) {
            set<int> lits = find_pure_literals();
            if (lits.size() == 0) break;
            for (int v : lits) acc->push_back(v);
            propagate_var_values(lits);
            if (has_empty_clause()) return false;
        }

        if (is_cnf_empty()) return true;

        size_t acclim = acc->size();
        int v = select_branching_literal();

        // branching
        {
            // Create copy in heap, do not eat stack
            CNF *cnf2 = new CNF(*this);
            acc->push_back(v);
            cnf2->propagate_var_single_value(v);
            bool ret = cnf2->solve_helper(acc);
            delete cnf2;
            if (ret) return ret;
        }

        // rollback and try again
        acc->erase(acc->begin() + (ssize_t)acclim, acc->end());

        acc->push_back(-v);
        propagate_var_single_value(-v);
        return solve_helper(acc);
    }

    vector<int>* dpll() {
        vector<int>* acc = new vector<int>();
        bool ret = solve_helper(acc);
        if (ret) {
            // Append missing variables
            std::sort(acc->begin(), acc->end(), [](int a, int b) { return abs(a) < abs(b); });
            int current = 1;
            for (size_t i = 0; i < acc->size(); i++) {
                while (current < abs(acc->at(i))) {
                    acc->push_back(current);
                    current++;
                }
                current++;
            }
            std::sort(acc->begin(), acc->end(), [](int a, int b) { return abs(a) < abs(b); });
            return acc;
        }
        delete acc;
        return nullptr;
    }
};

int main(int argc, char* argv[]) {
    if (argc != 2) {
        std::cout << "Usage: " << argv[0] << " <filename>\n";
        return 1;
    }
    CNF *c = CNF::parse(argv[1]);
    if (!c) return 1;
    vector<int>* ret = c->dpll();
    delete c;
    if (ret) {
        std::cout << "v ";
        for (int v : *ret) std::cout << v << " ";
        std::cout << "0\n";
        delete ret;
    } else {
        std::cout << "UNSAT\n";
    }
    return 0;
}
