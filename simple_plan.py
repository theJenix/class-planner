#!/usr/bin/env python

class ConstraintSatisfactionProblem(object):
    def __init__(self, variables, domainFn):
        """
            Construct a constraint satisfaction problem with an array of
            variables and a domain function.  The domain function allows
            for arbitrary complex "preselection" and preordering of
            values
        """
        self.variables = variables
        self.domainFn = domainFn
    
    def getPotentialAssignments(self, var):
        """
            Get potential assignments for this variable from the domain
            funciton 
        """
        return self.domainFn(var)
        
    def isValueConsistent(self, assignment, var, val):
        """
            Check to see the value is consistent with the assignment
            For now, we will assume it is if we don't have an assignment
            for that variable yet. 
        """
        if assignment.isAssigned(var, val):
            print {var:val}, "inconsistent with", assignment.assignment
        return not assignment.isAssigned(var, val)

    def emptyAssignment(self):
        return Assignment(self)

    def inference(self, var, value):
        """
            Make inferences on the CSP based on this variable assignment
            For now, we will use this to enforce breaks in between classes
        """
        inx = self.variables.index(var)
        return {self.variables[inx + 1] : 'BREAK'} if inx < (len(self.variables) - 1) else {}

class Assignment(object):
    def __init__(self, csp):
        self.variables = csp.variables
        self.assignment = {}
        self.currentVariableIndex = 0
        self.currentVariable = self.variables[self.currentVariableIndex]

    def addInferences(self, inferences):
        print "Adding inferences", inferences

        self.assignment.update(inferences)
        print self.assignment

    def getAssignment(self, var):
        return self.assignment[var] if var in self.assignment else None
        
    def isAssigned(self, var, value):
        return var in self.assignment or value in self.assignment.values()

    def isComplete(self):
        """
            Test if the assignment is complete by checking to see if all of the
            variables are assigned to something.
        """
        print "Is Variable Assigned?", {v: (True if v in self.assignment else False) for v in self.variables}
        return all([True if v in self.assignment else False for v in self.variables])

    def makeAssignment(self, var, value):
        """
            Make an assignment
        """
        print "Assigning", var, "to", value
        self.assignment[var] = value

    def nextVariable(self):
        """
            Advance to the next variable and return it
        """
      
        while self.currentVariableIndex < len(self.variables) and  self.variables[self.currentVariableIndex] in self.assignment:
            self.currentVariableIndex += 1  
        if self.currentVariableIndex < len(self.variables):
            self.currentVariable = self.variables[self.currentVariableIndex]
        else:
            self.currentVariable = None
        return self.currentVariable

    def removeAssignment(self, var):
        del self.assignment[var]
        
    def removeInferences(self, inferences):
        for k in inferences:
            del self.assignment[k]

def backtrack(csp):
    return doBacktrack(csp.emptyAssignment(), csp)

def doBacktrack(assignment, csp):
    if assignment.isComplete():
        print "Complete assignment!"
        return assignment

    var = assignment.nextVariable()
    if not var:
        print "Out of variables"
        return None

    print "Processing", var

    for value in csp.getPotentialAssignments(var):
        if csp.isValueConsistent(assignment, var, value):
            assignment.makeAssignment(var, value)
            inferences = csp.inference(var, value)
            if not inferences == None:
                assignment.addInferences(inferences)
                result = doBacktrack(assignment, csp)
                if result:
                    return result
            assignment.removeInferences(inferences)
            assignment.removeAssignment(var)
    return None

def main():
    times = [13, 14, 15, 16, 17]
    classes = {13: ['CS1100', 'CS1331',  'CS4495'], 14: ['CS4001'], 15: ['CS4001', 'CS4290', 'CS4605'], 16: ['CS4641', 'CS1332'], 17: ['CS4001', 'CS2340']}
    csp = ConstraintSatisfactionProblem(times, lambda time: classes[time])

    assignment = backtrack(csp)
    print "Your schedule:"
    for time in times:
        print "At time", time, "you have", assignment.getAssignment(time)

if __name__ == "__main__":
    main()
